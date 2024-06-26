#![allow(dead_code)]
#[macro_use]
extern crate pest_derive;
use crate::cgo::Trace;
use anyhow::*;
use compiler::ConstraintSet;
use errno::{set_errno, Errno};
use libc::c_char;
use log::*;
use rayon::ThreadPool;
use std::{
    ffi::{c_uint, CStr, CString},
    sync::RwLock,
};

mod cgo;
mod check;
mod column;
mod compiler;
mod compute;
mod constants;
mod dag;
mod errors;
mod import;
mod pretty;
mod structs;
mod transformer;
mod utils;

pub(crate) static IS_NATIVE: RwLock<bool> = RwLock::new(true);

type Corset = ConstraintSet;

#[derive(Copy, Clone)]
#[repr(i32)]
enum CorsetError {
    NotAnUsize = 1,
    ComputeTraceFailed,
    ColumnNameNotFound,
    ColumnIdNotFound,
    InitializingRayon,
    InvalidZkEvmFile,
    CheckFailed,
    EmptyTrace,
    NotAnError,
}
impl From<i32> for CorsetError {
    fn from(x: i32) -> Self {
        match x {
            a if a == CorsetError::NotAnUsize as i32 => CorsetError::NotAnUsize,
            a if a == CorsetError::ComputeTraceFailed as i32 => CorsetError::ComputeTraceFailed,
            a if a == CorsetError::ColumnNameNotFound as i32 => CorsetError::ColumnNameNotFound,
            a if a == CorsetError::ColumnIdNotFound as i32 => CorsetError::ColumnIdNotFound,
            a if a == CorsetError::InitializingRayon as i32 => CorsetError::InitializingRayon,
            a if a == CorsetError::InvalidZkEvmFile as i32 => CorsetError::InvalidZkEvmFile,
            a if a == CorsetError::CheckFailed as i32 => CorsetError::CheckFailed,
            a if a == CorsetError::EmptyTrace as i32 => CorsetError::EmptyTrace,
            _ => CorsetError::NotAnError,
        }
    }
}
impl From<CorsetError> for Errno {
    fn from(e: CorsetError) -> Self {
        Errno(e as i32)
    }
}
impl std::fmt::Display for CorsetError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CorsetError::NotAnUsize => write!(f, "could not convert to usize"),
            CorsetError::ComputeTraceFailed => write!(f, "failed to expand the trace"),
            CorsetError::ColumnNameNotFound => write!(f, "column name not found"),
            CorsetError::ColumnIdNotFound => write!(f, "column ID not found"),
            CorsetError::InitializingRayon => write!(f, "failed to initialize rayon"),
            CorsetError::InvalidZkEvmFile => write!(f, "invalid zkEVM constraints file"),
            CorsetError::CheckFailed => write!(f, "the trace does not satisfy the constraints"),
            CorsetError::EmptyTrace => write!(f, "refusing to process an empty trace"),
            CorsetError::NotAnError => write!(f, "this is not a valid Corset error"),
        }
    }
}

fn cstr_to_string<'a>(s: *const c_char) -> &'a str {
    let name = unsafe {
        assert!(!s.is_null());
        CStr::from_ptr(s)
    };

    name.to_str().unwrap()
}

#[no_mangle]
pub extern "C" fn corset_from_file(zkevmfile: *const c_char) -> *mut Corset {
    let zkevmfile = cstr_to_string(zkevmfile);
    match cgo::corset_from_file(zkevmfile) {
        Result::Ok(constraints) => {
            set_errno(Errno(0));
            Box::into_raw(Box::new(constraints))
        }
        Err(e) => {
            eprintln!("{:?}", e);
            set_errno(Errno(CorsetError::InvalidZkEvmFile as i32));
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
pub extern "C" fn corset_from_string(zkevmstr: *const c_char) -> *mut Corset {
    let zkevmstr = cstr_to_string(zkevmstr);
    match cgo::corset_from_str(zkevmstr) {
        Result::Ok(constraints) => {
            set_errno(Errno(0));
            Box::into_raw(Box::new(constraints))
        }
        Err(e) => {
            eprintln!("{:?}", e);
            set_errno(CorsetError::InvalidZkEvmFile.into());
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
            set_errno(CorsetError::NotAnUsize.into());
            return false;
        })
        .build()
        .is_err()
    {
        set_errno(CorsetError::InitializingRayon.into());
        return false;
    }

    let corset = Corset::mut_from_ptr(corset);
    let tracefile = cstr_to_string(tracefile);

    match _trace_check(corset, tracefile, fail_on_missing) {
        Result::Ok(_) => true,
        Err(e) => {
            eprintln!("{e:?}");
            set_errno(CorsetError::CheckFailed.into());
            false
        }
    }
}

fn init_rayon(threads: c_uint) -> Result<ThreadPool> {
    match rayon::ThreadPoolBuilder::new()
        .num_threads(if let Result::Ok(t) = threads.try_into() {
            t
        } else {
            set_errno(CorsetError::NotAnUsize.into());
            bail!("not an usize");
        })
        .build()
    {
        Err(e) => {
            set_errno(CorsetError::InitializingRayon.into());
            bail!(e)
        }
        Result::Ok(tp) => Ok(tp),
    }
}

#[no_mangle]
pub extern "C" fn trace_compute_from_file(
    corset: *mut Corset,
    tracefile: *const c_char,
    threads: c_uint,
    fail_on_missing: bool,
) -> *mut Trace {
    match init_rayon(threads) {
        Result::Ok(tp) => {
            let tracefile = cstr_to_string(tracefile);
            let constraints = Corset::mut_from_ptr(corset);
            let r = tp
                .install(|| cgo::compute_trace_from_file(constraints, tracefile, fail_on_missing));
            match r {
                Err(e) => {
                    eprintln!("{:?}", e);
                    set_errno(CorsetError::ComputeTraceFailed.into());
                    std::ptr::null_mut()
                }
                Result::Ok(x) => {
                    set_errno(Errno(0));
                    Box::into_raw(Box::new(x))
                }
            }
        }
        Err(_) => std::ptr::null_mut(),
    }
}

#[no_mangle]
pub extern "C" fn trace_compute_from_string(
    corset: *mut Corset,
    tracestr: *const c_char,
    threads: c_uint,
    fail_on_missing: bool,
) -> *mut Trace {
    match init_rayon(threads) {
        Result::Ok(tp) => {
            let tracestr = cstr_to_string(tracestr);
            if tracestr.is_empty() {
                set_errno(CorsetError::EmptyTrace.into());
                return std::ptr::null_mut();
            }

            let constraints = Corset::mut_from_ptr(corset);
            let r =
                tp.install(|| cgo::compute_trace_from_str(constraints, tracestr, fail_on_missing));
            match r {
                Err(e) => {
                    eprintln!("{:?}", e);
                    set_errno(CorsetError::ComputeTraceFailed.into());
                    std::ptr::null_mut()
                }
                Result::Ok(x) => {
                    set_errno(Errno(0));
                    Box::into_raw(Box::new(x))
                }
            }
        }
        Err(_) => {
            set_errno(CorsetError::InitializingRayon.into());
            std::ptr::null_mut()
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
    padding_value: [u8; 32],
    values: *const [u8; 32],
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
        set_errno(CorsetError::ColumnNameNotFound.into());
        r
    }
}

#[no_mangle]
pub extern "C" fn trace_column_by_id(trace: *const Trace, i: u32) -> ColumnData {
    let r = Trace::from_ptr(trace);
    let i = i as usize;
    assert!(i < r.columns.len());
    if let Some(col) = r.columns.get(i) {
        if col.is_empty() {
            panic!("FREED COLUMN")
        } else if col.values.is_empty() {
            // A non-allocated Vec return an elt-aligned pointer, here 0x8
            // typically. However, Go twists his panties in a bunch if it merely
            // sees an invalid pointer on the stack. Therefore, we have to
            // return a null pointer instead of an empty vec in this case.
            Default::default()
        } else {
            ColumnData {
                padding_value: col.padding_value,
                values: col.values.as_ptr(),
                values_len: col.values.len() as u64,
            }
        }
    } else {
        set_errno(CorsetError::ColumnIdNotFound.into());
        ColumnData::default()
    }
}

#[no_mangle]
pub extern "C" fn free_column_by_name(trace: *mut Trace, name: *const c_char) {
    let r = Trace::mut_from_ptr(trace);
    let name = cstr_to_string(name);

    let i = r.ids.iter().position(|n| *n == name);
    if let Some(i) = i {
        r.columns[i] = cgo::ComputedColumn::empty();
    } else {
        set_errno(CorsetError::ColumnNameNotFound.into());
    }
}

#[no_mangle]
pub extern "C" fn corset_err_to_string(err: i32) -> *mut c_char {
    let err: CorsetError = err.into();
    CString::new(err.to_string()).unwrap().into_raw()
}
