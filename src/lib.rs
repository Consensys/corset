#![allow(dead_code)]
#[macro_use]
#[cfg(feature = "interactive")]
extern crate pest_derive;
use anyhow::*;
use compiler::ConstraintSet;
use libc::c_char;
use log::*;
use pairing_ce::{
    bn256::Fr,
    ff::{Field, PrimeField},
};
use std::ffi::{c_uint, CStr, CString};

use crate::{column::Computation, compiler::EvalSettings, structs::Handle};

mod column;
mod compiler;
mod compute;
mod dag;
mod errors;
mod pretty;
mod structs;
mod transformer;

fn cstr_to_string(s: *const c_char) -> String {
    let name = unsafe {
        assert!(!s.is_null());
        CStr::from_ptr(s)
    };

    name.to_str().unwrap().to_owned()
}

struct ComputedColumn {
    padding_value: [u64; 4],
    values: Vec<[u64; 4]>,
}
#[derive(Default)]
pub struct ColumnsRegister {
    columns: Vec<ComputedColumn>,
    ids: Vec<String>,
}
impl ColumnsRegister {
    fn from_constraints(c: &ConstraintSet) -> Self {
        let mut r = ColumnsRegister {
            ..Default::default()
        };

        for (module, columns) in c.modules.cols.iter() {
            let empty_vec = Vec::new();
            for (name, &i) in columns.iter() {
                let column = &c.modules._cols[i];
                let handle = Handle::new(&module, &name);
                let value = column.value().unwrap_or(&empty_vec);
                let padding = if let Some(x) = column.padding_value {
                    Fr::from_str(&x.to_string()).unwrap()
                } else {
                    value.get(0).cloned().unwrap_or_else(|| {
                        c.computations
                            .computation_for(&handle)
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
                r.columns.push(ComputedColumn {
                    values: column
                        .value()
                        .unwrap_or(&empty_vec)
                        .iter()
                        .map(|x| x.into_repr().0)
                        .collect(),
                    padding_value: padding.into_repr().0,
                });
                r.ids.push(handle.mangle());
            }
        }
        r
    }
    fn from_ptr<'a>(ptr: *const ColumnsRegister) -> &'a Self {
        assert!(!ptr.is_null());
        unsafe { &*ptr }
    }
}

fn _compute_trace(
    zkevmfile: &str,
    tracefile: &str,
    fail_on_missing: bool,
) -> Result<ColumnsRegister> {
    info!("Loading `{}`", &zkevmfile);
    let mut constraints = ron::from_str(
        &std::fs::read_to_string(&zkevmfile)
            .with_context(|| anyhow!("while reading `{}`", zkevmfile))?,
    )
    .with_context(|| anyhow!("while parsing `{}`", zkevmfile))?;

    transformer::validate_nhood(&mut constraints)?;
    transformer::lower_shifts(&mut constraints)?;
    transformer::expand_ifs(&mut constraints);
    transformer::expand_constraints(&mut constraints)?;
    transformer::sorts(&mut constraints)?;
    transformer::expand_invs(&mut constraints)?;

    compute::compute_trace(
        &compute::read_trace(&tracefile)?,
        &mut constraints,
        fail_on_missing,
    )
    .with_context(|| format!("while computing from `{}`", tracefile))?;
    Ok(ColumnsRegister::from_constraints(&constraints))
}

#[no_mangle]
pub extern "C" fn trace_compute(
    zkevmfile: *const c_char,
    tracefile: *const c_char,
    threads: c_uint,
    fail_on_missing: bool,
) -> *mut ColumnsRegister {
    rayon::ThreadPoolBuilder::new()
        .num_threads(threads.try_into().expect("not an usize"))
        .build_global()
        .expect("failed to initialize rayon");

    let zkevmfile = cstr_to_string(zkevmfile);
    let tracefile = cstr_to_string(tracefile);
    let r = _compute_trace(&zkevmfile, &tracefile, fail_on_missing);
    match r {
        Err(e) => {
            eprintln!("{:?}", e);
            panic!();
        }
        core::result::Result::Ok(x) => Box::into_raw(Box::new(x)),
    }
}

#[no_mangle]
pub extern "C" fn trace_free(ptr: *mut ColumnsRegister) {
    if !ptr.is_null() {
        unsafe {
            drop(Box::from_raw(ptr));
        }
    }
}

#[no_mangle]
pub extern "C" fn trace_column_count(ptr: *const ColumnsRegister) -> c_uint {
    let r = ColumnsRegister::from_ptr(ptr);
    r.ids.len() as c_uint
}

#[no_mangle]
pub extern "C" fn trace_column_names(ptr: *const ColumnsRegister) -> *const *mut c_char {
    let r = ColumnsRegister::from_ptr(ptr);
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

#[no_mangle]
pub extern "C" fn trace_column_by_name(
    ptr: *const ColumnsRegister,
    name: *const c_char,
) -> ColumnData {
    let r = ColumnsRegister::from_ptr(ptr);
    let name = cstr_to_string(name);

    let i = r
        .ids
        .iter()
        .position(|n| *n == name)
        .expect("unknown column name");

    trace_column_by_id(ptr, i.try_into().unwrap())
}

#[no_mangle]
pub extern "C" fn trace_column_by_id(ptr: *const ColumnsRegister, i: u32) -> ColumnData {
    let r = ColumnsRegister::from_ptr(ptr);
    let i = i as usize;
    assert!(i < r.columns.len());
    let col = &r.columns[i];

    ColumnData {
        padding_value: col.padding_value.into(),
        values: col.values.as_ptr(),
        values_len: col.values.len() as u64,
    }
}
