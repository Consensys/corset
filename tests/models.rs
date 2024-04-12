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
    /// The oracle determines, for a given set of column data, whether
    /// or not it should be accepted or rejected.
    oracle: fn(data: &Trace) -> bool,
}

impl Model {
    const MIN_ELEMENT: isize = -1;
    const MAX_ELEMENT: isize = 1;

    /// Generate all traces matching the model configuration upto
    /// length `n`, and split them into the `accepts` and `rejects`.
    /// The former are those traces which are expected to pass, whilst
    /// the latter are those which are expected to fail.
    pub fn generate_traces_upto(&self, n: usize) -> (Vec<Trace>, Vec<Trace>) {
        let mut accepts = Vec::new();
        let mut rejects = Vec::new();
        //
        for i in 0..n {
            for tr in self.generate_all_traces(i) {
                // Test the trace using the given oracle to check whether
                // (or not) it should be accepted.
                if (self.oracle)(&tr) {
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
}

impl Trace {
    pub fn new(cols: &'static [&'static str], data: Vec<isize>) -> Self {
        Self { cols, data }
    }
    /// Determine how many rows of data there are.
    pub fn height(&self) -> usize {
        self.data.len() / self.width()
    }

    /// Determine how many columns of data there are.
    pub fn width(&self) -> usize {
        self.cols.len()
    }

    /// Access the data for a given column.
    pub fn get(&self, col: usize) -> &[isize] {
        let n = self.height();
        let start = col * n;
        let end = (col + 1) * n;
        &self.data[start..end]
    }
}

impl Index<&str> for Trace {
    type Output = [isize];

    fn index(&self, index: &str) -> &Self::Output {
        for i in 0..self.cols.len() {
            if self.cols[i] == index {
                return self.get(i);
            }
        }
        panic!("unknown column: {index}");
    }
}

// ===================================================================
// Models
// ===================================================================

/// The master list of active models.  Tests will be automatically
/// generated for each item in this list.
static MODELS: &[Model] = &[
    // Model {name: "arrays_1", cols: &["A", "B_1","B_2","B_3"], oracle: arrays_1_oracle},
    Model {
        name: "iszero",
        cols: &["A", "B"],
        oracle: iszero_oracle,
    },
    Model {
        name: "shift_1",
        cols: &["A", "B"],
        oracle: shift_1_oracle,
    },
    //Model {name: "shift_2", cols: &["A", "B"], oracle: shift_2_oracle} problem with padding
    Model {
        name: "shift_3",
        cols: &["A", "B"],
        oracle: shift_3_oracle,
    },
];

// ===================================================================
// Arrays
// ===================================================================

fn arrays_1_oracle(tr: &Trace) -> bool {
    for k in 0..tr.height() {
        let c1 = tr["A"][k] == 0 || (tr["B_1"][k] + 1 == tr["B_2"][k]);
        let c2 = tr["A"][k] == 0 || (tr["B_2"][k] + 2 == tr["B_3"][k]);
        let c3 =
            tr["A"][k] == tr["B_1"][k] || tr["A"][k] == tr["B_2"][k] || tr["A"][k] == tr["B_3"][k];
        if !(c1 && c2 && c3) {
            return false;
        }
    }
    true
}

// ===================================================================
// IsZero
// ===================================================================

fn iszero_oracle(tr: &Trace) -> bool {
    for k in 0..tr.height() {
        if tr["B"][k] != 0 && tr["A"][k] == 0 {
            return false;
        }
    }
    true
}

// ===================================================================
// Shift
// ===================================================================

fn shift_1_oracle(tr: &Trace) -> bool {
    for k in 0..tr.height() {
        let c1 = k + 1 >= tr.height() || tr["B"][k] == 0 || tr["A"][k] + 1 == tr["A"][k + 1];
        if !c1 {
            return false;
        }
    }
    true
}

fn shift_2_oracle(tr: &Trace) -> bool {
    for k in 0..tr.height() {
        let c1 = k == 0 || tr["B"][k] == 0 || tr["B"][k] == tr["A"][k - 1];
        if !c1 {
            return false;
        }
    }
    true
}

fn shift_3_oracle(tr: &Trace) -> bool {
    for k in 0..tr.height() {
        let c1 = k + 2 >= tr.height() || tr["B"][k] == 0 || tr["B"][k] == tr["A"][k + 2];
        if !c1 {
            return false;
        }
    }
    true
}
