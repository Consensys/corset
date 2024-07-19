use std::ops::Index;

/// For a given input file, the model determines which traces should
/// be accepted and which should be rejected.  In essence, we are
/// comparing a Rust function (the model) against the constraints
/// evaluated on the trace.
struct Model {
    /// The name of this test (which should correspond to a lisp
    /// file).
    name: &'static str,
    /// The column names needed for this test.
    cols: &'static [&'static str],
    /// Number of rows to generate for.
    limit: usize,
    /// The oracle determines, for a given set of column data, whether
    /// or not it should be accepted or rejected.
    oracle: Option<fn(data: &Trace) -> bool>,
}

impl Model {
    const MIN_ELEMENT: isize = -1;
    const MAX_ELEMENT: isize = 1;

    pub fn new(
        name: &'static str,
        cols: &'static [&'static str],
        limit: usize,
        oracle: Option<fn(data: &Trace) -> bool>,
    ) -> Self {
        Self {
            name,
            cols,
            limit,
            oracle,
        }
    }

    /// Generate all traces matching the model configuration upto
    /// length `n`, and split them into the `accepts` and `rejects`.
    /// The former are those traces which are expected to pass, whilst
    /// the latter are those which are expected to fail.
    pub fn generate_traces_upto(&self, n: usize) -> (Vec<Trace>, Vec<Trace>) {
        let Some(oracle) = self.oracle else {
            panic!();
        };
        let mut accepts = Vec::new();
        let mut rejects = Vec::new();
        //
        for i in 0..n {
            for tr in self.generate_all_traces(i) {
                // Test the trace using the given oracle to check whether
                // (or not) it should be accepted.
                if (oracle)(&tr) {
                    accepts.push(tr);
                } else {
                    rejects.push(tr);
                }
            }
        }
        // Done
        (accepts, rejects)
    }

    /// Generate all possible traces matching the model configuration
    /// of length `n`.
    fn generate_all_traces(&self, n: usize) -> Vec<Trace> {
        let width = self.cols.len();
        let mut tmp = vec![Self::MIN_ELEMENT; width * n];
        let mut data = Vec::new();
        // Initial row
        data.push(Trace::new(self.cols, tmp.clone()));
        // Add remaining rows
        while Self::next_trace(&mut tmp) {
            data.push(Trace::new(self.cols, tmp.clone()));
        }
        data
    }

    fn next_trace(data: &mut [isize]) -> bool {
        let mut i = 0;
        //
        while i < data.len() {
            if data[i] == Self::MAX_ELEMENT {
                data[i] = Self::MIN_ELEMENT;
                i = i + 1;
            } else {
                data[i] += 1;
                return true;
            }
        }
        // no more
        return false;
    }
}

/// Represents an individial trace which, for a given number of rows,
/// contains values for each of the columns.
struct Trace {
    /// Simplistic column schema.
    cols: &'static [&'static str],
    /// The trace data, whose length must be divisible by `width`.
    /// Data is stored one column after another.
    data: Vec<isize>,
    /// Identifies how many rows of padding come at the beginning.
    padding: usize,
}

impl Trace {
    pub fn new(cols: &'static [&'static str], data: Vec<isize>) -> Self {
        Self {
            cols,
            data,
            padding: 1,
        }
    }
    /// Determine how many rows of data there are.
    pub fn height(&self) -> usize {
        self.padding + (self.data.len() / self.width())
    }

    /// Determine how many columns of data there are.
    pub fn width(&self) -> usize {
        self.cols.len()
    }

    /// Access the data for a given column based on its name.
    pub fn col<'a>(&'a self, name: &str) -> Column<'a> {
        for i in 0..self.cols.len() {
            if self.cols[i] == name {
                return Column {
                    data: self.get(i),
                    padding: self.padding,
                };
            }
        }
        panic!("unknown column: {name}");
    }

    /// Access the data for a given column.
    pub fn get<'a>(&'a self, col: usize) -> &[isize] {
        let n = self.data.len() / self.width();
        let start = col * n;
        let end = (col + 1) * n;
        &self.data[start..end]
    }
}

/// Represents a single column which can contain zero (or more) rows
/// of padding at the beginning.
struct Column<'a> {
    // The raw data underlying this column
    data: &'a [isize],
    // The number of rows of passing which are assumed to be prepended
    // onto this column.
    padding: usize,
}

impl<'a> Index<usize> for Column<'a> {
    type Output = isize;

    fn index(&self, index: usize) -> &Self::Output {
        if index < self.padding {
            &0
        } else {
            &self.data[index - self.padding]
        }
    }
}

// ===================================================================
// Models
// ===================================================================

/// The master list of active models.  Tests will be automatically
/// generated for each item in this list.
static MODELS: &[Model] = &[
    Model {
        name: "arrays_1",
        cols: &["A", "B_1", "B_2", "B_3"],
        limit: 2,
        oracle: Some(arrays_1_oracle),
    },
    Model {
        name: "iszero",
        cols: &["A", "B"],
        limit: 3,
        oracle: Some(iszero_oracle),
    },
    Model {
        name: "shift_1",
        cols: &["A", "B"],
        limit: 3,
        oracle: Some(shift_1_oracle),
    },
    Model {
        name: "shift_2",
        cols: &["A", "B"],
        limit: 3,
        oracle: Some(shift_2_oracle),
    },
    Model {
        name: "shift_3",
        cols: &["A", "B"],
        limit: 3,
        oracle: Some(shift_3_oracle),
    },
    Model {
        name: "shift_5",
        cols: &["A", "B", "C"],
        limit: 2,
        oracle: Some(shift_5_oracle),
    },
    Model {
        name: "vanish_1",
        cols: &["X"],
        limit: 3,
        oracle: Some(|_| false),
    },
    Model {
        name: "vanish_2",
        cols: &["X"],
        limit: 3,
        oracle: Some(|_| false),
    },
];

// ===================================================================
// Arrays
// ===================================================================

#[allow(non_snake_case)]
fn arrays_1_oracle(tr: &Trace) -> bool {
    let (A, B_1, B_2, B_3) = (tr.col("A"), tr.col("B_1"), tr.col("B_2"), tr.col("B_3"));

    for k in 0..tr.height() {
        let c1 = A[k] == 0 || (B_1[k] + 1 == B_2[k]);
        let c2 = A[k] == 0 || (B_2[k] + 2 == B_3[k]);
        let c3 = A[k] == B_1[k] || A[k] == B_2[k] || A[k] == B_3[k];
        if !(c1 && c2 && c3) {
            return false;
        }
    }
    true
}

// // ===================================================================
// // IsZero
// // ===================================================================

#[allow(non_snake_case)]
fn iszero_oracle(tr: &Trace) -> bool {
    let (A, B) = (tr.col("A"), tr.col("B"));

    for k in 0..tr.height() {
        if B[k] != 0 && A[k] == 0 {
            return false;
        }
    }
    true
}

// // ===================================================================
// // Shift
// // ===================================================================

#[allow(non_snake_case)]
fn shift_1_oracle(tr: &Trace) -> bool {
    let (A, B) = (tr.col("A"), tr.col("B"));

    for k in 0..tr.height() {
        let c1 = k + 1 >= tr.height() || B[k] == 0 || A[k] + 1 == A[k + 1];
        if !c1 {
            return false;
        }
    }
    true
}

#[allow(non_snake_case)]
fn shift_2_oracle(tr: &Trace) -> bool {
    let (A, B) = (tr.col("A"), tr.col("B"));

    for k in 0..tr.height() {
        let c1 = k == 0 || B[k] == 0 || B[k] == A[k - 1];
        if !c1 {
            return false;
        }
    }
    true
}

#[allow(non_snake_case)]
fn shift_3_oracle(tr: &Trace) -> bool {
    let (A, B) = (tr.col("A"), tr.col("B"));

    for k in 0..tr.height() {
        let c1 = k + 2 >= tr.height() || B[k] == 0 || B[k] == A[k + 2];
        if !c1 {
            return false;
        }
    }
    true
}

#[allow(non_snake_case)]
fn shift_5_oracle(tr: &Trace) -> bool {
    let (A, B, C) = (tr.col("A"), tr.col("B"), tr.col("C"));

    for k in 0..tr.height() {
        let c1 = k < 4 || A[k - 4] == 0;
        let c2 = k < 4 || A[k - 4] + C[k - 1] == 0;
        if !c1 || !c2 {
            return false;
        }
    }
    true
}
