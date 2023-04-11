#![allow(dead_code)]
#[macro_use]
#[cfg(feature = "interactive")]
extern crate pest_derive;
use anyhow::*;
use compiler::ConstraintSet;
use errno::{set_errno, Errno};
use libc::c_char;
use log::*;
use pairing_ce::{
    bn256::Fr,
    ff::{Field, PrimeField},
};
use rayon::prelude::*;
use std::ffi::{c_uint, CStr, CString};

use crate::{column::Computation, compiler::EvalSettings};

mod check;
mod column;
mod compiler;
mod compute;
mod dag;
mod errors;
#[cfg_attr(
    all(target_arch = "x86_64", target_feature = "avx"),
    path = "import_simd.rs"
)]
#[cfg_attr(not(all(target_arch = "x86_64")), path = "import.rs")]
mod import;
mod pretty;
mod structs;
mod transformer;
mod utils;

type Corset = ConstraintSet;

pub const ERR_NOT_AN_USIZE: i32 = 1;
pub const ERR_COMPUTE_TRACE_FAILED: i32 = 2;
pub const ERR_COLUMN_NAME_NOT_FOUND: i32 = 3;
pub const ERR_COULD_NOT_INITIALIZE_RAYON: i32 = 4;
pub const ERR_COLUMN_ID_NOT_FOUND: i32 = 5;
pub const ERR_INVALID_ZKEVM_FILE: i32 = 6;
pub const ERR_CHECK_FAILED: i32 = 7;

fn cstr_to_string<'a>(s: *const c_char) -> &'a str {
    let name = unsafe {
        assert!(!s.is_null());
        CStr::from_ptr(s)
    };

    name.to_str().unwrap()
}

struct ComputedColumn {
    padding_value: [u64; 4],
    values: Vec<[u64; 4]>,
}
#[derive(Default)]
pub struct Trace {
    columns: Vec<ComputedColumn>,
    ids: Vec<String>,
}
impl Trace {
    fn from_constraints(c: &Corset, convert_to_be: bool) -> Self {
        let mut r = Trace {
            ..Default::default()
        };

        let rs = c
            .columns
            .all()
            .par_iter()
            .map(|i| {
                let empty_vec = Vec::new();
                let column = &c.columns._cols[i.as_id()];
                let value = c.columns.value(i.into()).unwrap_or(&empty_vec);
                let padding = if let Some(x) = column.padding_value {
                    Fr::from_str(&x.to_string()).unwrap()
                } else {
                    value.get(0).cloned().unwrap_or_else(|| {
                        c.computations
                            .computation_for(i.into())
                            .map(|c| match c {
                                Computation::Composite { exp, .. } => exp
                                    .eval(
                                        0,
                                        &mut |_, _, _| Some(Fr::zero()),
                                        &mut None,
                                        &EvalSettings::default(),
                                    )
                                    .unwrap_or_else(Fr::zero),
                                Computation::Interleaved { .. } => Fr::zero(),
                                Computation::Sorted { .. } => Fr::zero(),
                                Computation::CyclicFrom { .. } => Fr::zero(),
                                Computation::SortingConstraints { .. } => Fr::zero(),
                            })
                            .unwrap_or_else(Fr::zero)
                    })
                };
                (
                    ComputedColumn {
                        values: c
                            .columns
                            .value(i)
                            .unwrap_or(&empty_vec)
                            .iter()
                            .map(|x| {
                                let mut v = x.into_repr().0;
                                if convert_to_be {
                                    reverse_fr(&mut v);
                                }
                                v
                            })
                            .collect(),
                        padding_value: {
                            let mut padding = padding.into_repr().0;
                            if convert_to_be {
                                reverse_fr(&mut padding);
                            }
                            padding
                        },
                    },
                    column.handle.to_string(),
                )
            })
            .collect::<Vec<_>>();

        for (col, id) in rs.into_iter() {
            r.columns.push(col);
            r.ids.push(id);
        }

        r
    }
    fn from_ptr<'a>(ptr: *const Trace) -> &'a Self {
        assert!(!ptr.is_null());
        unsafe { &*ptr }
    }
}

fn reverse_fr(v: &mut [u64; 4]) {
    #[cfg(target_arch = "aarch64")]
    reverse_fr_aarch64(v);
    #[cfg(target_arch = "x86_64")]
    reverse_fr_x86_64(v);
    #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
    reverse_fr_fallback(v);
}

fn reverse_fr_fallback(v: &mut [u64; 4]) {
    for vi in v.iter_mut() {
        *vi = vi.swap_bytes();
    }
    v.swap(0, 3);
    v.swap(1, 2);
}

#[cfg(target_arch = "aarch64")]
fn reverse_fr_aarch64(v: &mut [u64; 4]) {
    for vi in v.iter_mut() {
        *vi = vi.swap_bytes();
    }
    v.swap(0, 3);
    v.swap(1, 2);
}

#[cfg(target_arch = "x86_64")]
fn reverse_fr_x86_64(v: &mut [u64; 4]) {
    if is_x86_feature_detected!("avx2") {
        unsafe {
            use std::arch::x86_64::*;
            let inverter = _mm256_set_epi64x(
                0x0001020304050607,
                0x08090a0b0c0d0e0f,
                0x0001020304050607,
                0x08090a0b0c0d0e0f,
            );
            let value = _mm256_loadu_si256(v.as_ptr() as *const __m256i);
            let x = _mm256_shuffle_epi8(value, inverter);
            *v = std::mem::transmute(_mm256_permute2f128_si256(x, x, 0x01));
        }
    } else {
        for vi in v.iter_mut() {
            *vi = vi.swap_bytes();
        }
        v.swap(0, 3);
        v.swap(1, 2);
    }
}

fn _corset_from_file(zkevmfile: &str) -> Result<Corset> {
    info!("Loading `{}`", &zkevmfile);
    let mut constraints = ron::from_str(
        &std::fs::read_to_string(zkevmfile)
            .with_context(|| anyhow!("while reading `{}`", zkevmfile))?,
    )
    .with_context(|| anyhow!("while parsing `{}`", zkevmfile))?;

    transformer::validate_nhood(&mut constraints)?;
    transformer::lower_shifts(&mut constraints);
    transformer::expand_ifs(&mut constraints);
    transformer::expand_constraints(&mut constraints)?;
    transformer::sorts(&mut constraints)?;
    transformer::expand_invs(&mut constraints)?;

    Ok(constraints)
}
fn _corset_from_str(zkevmstr: &str) -> Result<Corset> {
    let mut constraints =
        ron::from_str(zkevmstr).with_context(|| anyhow!("while parsing the provided zkEVM"))?;

    transformer::validate_nhood(&mut constraints)?;
    transformer::lower_shifts(&mut constraints);
    transformer::expand_ifs(&mut constraints);
    transformer::expand_constraints(&mut constraints)?;
    transformer::sorts(&mut constraints)?;
    transformer::expand_invs(&mut constraints)?;

    Ok(constraints)
}

fn _compute_trace_from_file(
    constraints: &mut Corset,
    tracefile: &str,
    convert_to_be: bool,
    fail_on_missing: bool,
) -> Result<Trace> {
    compute::compute_trace(tracefile, constraints, fail_on_missing)
        .with_context(|| format!("while computing from `{}`", tracefile))?;
    Ok(Trace::from_constraints(constraints, convert_to_be))
}

fn _compute_trace_from_str(
    constraints: &mut Corset,
    tracestr: &str,
    convert_to_be: bool,
    fail_on_missing: bool,
) -> Result<Trace> {
    compute::compute_trace_str(tracestr.as_bytes(), constraints, fail_on_missing)
        .with_context(|| format!("while computing from `{}`", tracestr))?;
    Ok(Trace::from_constraints(constraints, convert_to_be))
}

#[no_mangle]
pub extern "C" fn corset_from_file(zkevmfile: *const c_char) -> *mut Corset {
    let zkevmfile = cstr_to_string(zkevmfile);
    match _corset_from_file(zkevmfile) {
        Result::Ok(constraints) => {
            set_errno(Errno(0));
            Box::into_raw(Box::new(constraints))
        }
        Err(e) => {
            eprintln!("{:?}", e);
            set_errno(Errno(ERR_INVALID_ZKEVM_FILE));
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
pub extern "C" fn corset_from_string(zkevmstr: *const c_char) -> *mut Corset {
    let zkevmstr = cstr_to_string(zkevmstr);
    match _corset_from_str(zkevmstr) {
        Result::Ok(constraints) => {
            set_errno(Errno(0));
            Box::into_raw(Box::new(constraints))
        }
        Err(e) => {
            eprintln!("{:?}", e);
            set_errno(Errno(ERR_INVALID_ZKEVM_FILE));
            std::ptr::null_mut()
        }
    }
}

fn _trace_check(corset: &mut ConstraintSet, tracefile: &str, fail_on_missing: bool) -> Result<()> {
    compute::compute_trace(tracefile, corset, fail_on_missing)
        .with_context(|| format!("while expanding `{}`", tracefile))?;

    check::check(
        corset,
        &None,
        &[],
        false,
        true,
        check::DebugSettings::new()
            .unclutter(false)
            .dim(true)
            .continue_on_error(false)
            .report(false)
            .full_trace(false),
    )
    .with_context(|| format!("while checking `{}`", tracefile))?;
    info!("{}: SUCCESS", tracefile);

    Ok(())
}

#[no_mangle]
pub extern "C" fn trace_check(
    corset: *mut Corset,
    tracefile: *const c_char,
    threads: c_uint,
    fail_on_missing: bool,
) -> bool {
    if rayon::ThreadPoolBuilder::new()
        .num_threads(if let Result::Ok(t) = threads.try_into() {
            t
        } else {
            set_errno(Errno(ERR_NOT_AN_USIZE));
            return false;
        })
        .build_global()
        .is_err()
    {
        set_errno(Errno(ERR_COULD_NOT_INITIALIZE_RAYON));
        return false;
    }

    let corset = Corset::mut_from_ptr(corset);
    let tracefile = cstr_to_string(tracefile);

    match _trace_check(corset, tracefile, fail_on_missing) {
        Result::Ok(_) => true,
        Err(e) => {
            eprintln!("{e:?}");
            set_errno(Errno(ERR_CHECK_FAILED));
            false
        }
    }
}

#[no_mangle]
pub extern "C" fn trace_compute_from_file(
    corset: *mut Corset,
    tracefile: *const c_char,
    threads: c_uint,
    convert_to_be: bool,
    fail_on_missing: bool,
) -> *mut Trace {
    if rayon::ThreadPoolBuilder::new()
        .num_threads(if let Result::Ok(t) = threads.try_into() {
            t
        } else {
            set_errno(Errno(ERR_NOT_AN_USIZE));
            return std::ptr::null_mut();
        })
        .build_global()
        .is_err()
    {
        set_errno(Errno(ERR_COULD_NOT_INITIALIZE_RAYON));
        return std::ptr::null_mut();
    }

    let tracefile = cstr_to_string(tracefile);
    let constraints = Corset::mut_from_ptr(corset);
    let r = _compute_trace_from_file(constraints, tracefile, convert_to_be, fail_on_missing);
    match r {
        Err(e) => {
            eprintln!("{:?}", e);
            set_errno(Errno(ERR_COMPUTE_TRACE_FAILED));
            std::ptr::null_mut()
        }
        Result::Ok(x) => {
            set_errno(Errno(0));
            Box::into_raw(Box::new(x))
        }
    }
}

#[no_mangle]
pub extern "C" fn trace_compute_from_string(
    corset: *mut Corset,
    tracestr: *const c_char,
    threads: c_uint,
    convert_to_be: bool,
    fail_on_missing: bool,
) -> *mut Trace {
    if rayon::ThreadPoolBuilder::new()
        .num_threads(if let Result::Ok(t) = threads.try_into() {
            t
        } else {
            set_errno(Errno(ERR_NOT_AN_USIZE));
            return std::ptr::null_mut();
        })
        .build_global()
        .is_err()
    {
        set_errno(Errno(ERR_COULD_NOT_INITIALIZE_RAYON));
        return std::ptr::null_mut();
    }

    let tracestr = cstr_to_string(tracestr);
    let constraints = Corset::mut_from_ptr(corset);
    let r = _compute_trace_from_str(constraints, tracestr, convert_to_be, fail_on_missing);
    match r {
        Err(e) => {
            eprintln!("{:?}", e);
            set_errno(Errno(ERR_COMPUTE_TRACE_FAILED));
            std::ptr::null_mut()
        }
        Result::Ok(x) => {
            set_errno(Errno(0));
            Box::into_raw(Box::new(x))
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn trace_free(trace: *mut Trace) {
    if !trace.is_null() {
        drop(Box::from_raw(trace));
    }
}

#[no_mangle]
pub extern "C" fn trace_column_count(trace: *const Trace) -> c_uint {
    let r = Trace::from_ptr(trace);
    r.ids.len() as c_uint
}

#[no_mangle]
pub extern "C" fn trace_column_names(trace: *const Trace) -> *const *mut c_char {
    let r = Trace::from_ptr(trace);
    let names = r
        .ids
        .iter()
        .map(|s| CString::new(s.as_str()).unwrap().into_raw())
        .collect::<Vec<_>>();
    let ptr = names.as_ptr();

    std::mem::forget(names); // so that it is not destructed at the end of the scope

    ptr
}

#[repr(C)]
pub struct ColumnData {
    padding_value: [u64; 4],
    values: *const [u64; 4],
    values_len: u64,
}
impl Default for ColumnData {
    fn default() -> Self {
        ColumnData {
            padding_value: Default::default(),
            values: std::ptr::null(),
            values_len: 0,
        }
    }
}

#[no_mangle]
pub extern "C" fn trace_column_by_name(trace: *const Trace, name: *const c_char) -> ColumnData {
    let r = Trace::from_ptr(trace);
    let name = cstr_to_string(name);

    let i = r.ids.iter().position(|n| *n == name);
    if let Some(i) = i {
        trace_column_by_id(trace, i.try_into().unwrap())
    } else {
        let r = Default::default();
        set_errno(Errno(ERR_COLUMN_NAME_NOT_FOUND));
        r
    }
}

#[no_mangle]
pub extern "C" fn trace_column_by_id(trace: *const Trace, i: u32) -> ColumnData {
    let r = Trace::from_ptr(trace);
    let i = i as usize;
    assert!(i < r.columns.len());
    let col = if let Some(c) = r.columns.get(i) {
        c
    } else {
        let r = ColumnData::default();
        set_errno(Errno(ERR_COLUMN_ID_NOT_FOUND));
        return r;
    };

    ColumnData {
        padding_value: col.padding_value,
        values: col.values.as_ptr(),
        values_len: col.values.len() as u64,
    }
}
