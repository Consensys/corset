package main

// ===================
// For dynamic linking
// ===================
// cgo LDFLAGS: -L./target/release/ -lcorset

// ==================
// For static linking
// ==================

//#cgo LDFLAGS: ./target/release/libcorset.a -lm
//#cgo CFLAGS: -I./target/
//#include <corset.h>
import "C"
import "fmt"
import "unsafe"
import _ "embed"

// +----------------------------------------------------------------------------------------+
// |                                                                                        |
// |  ==========  Exposed by libcorset ==========                                           |
// |                                                                                        |
// |                                                                                        |
// |  * Compute a trace & free it                                                           |
// |                                                                                        |
// |  struct Trace *trace_compute(const char *zkevmfile,                                    |
// |                              const char *tracefile,                                    |
// |                              unsigned int threads,                                     |
// |                              bool fail_on_missing);                                    |
// |  void trace_free(struct Trace *trace);                                                 |
// |                                                                                        |
// |                                                                                        |
// |  * Get the number of columns in the trace                                              |
// |                                                                                        |
// |  unsigned int trace_column_count(const struct Trace *trace);                           |
// |                                                                                        |
// |                                                                                        |
// |  * Get all column names; position in array == ID of the column                         |
// |                                                                                        |
// |  char *const *trace_column_names(const struct Trace *trace);                           |
// |                                                                                        |
// |                                                                                        |
// |  * Get column values, either by name or by ID (faster)                                 |
// |                                                                                        |
// |  struct ColumnData trace_column_by_name(const struct Trace *trace, const char *name);  |
// |  struct ColumnData trace_column_by_id(const struct Trace *trace, uint32_t i);          |
// |                                                                                        |
// |  * What's in a column                                                                  |
// |                                                                                        |
// |  typedef struct ColumnData {    // all Fr are represented as [uint64; 4]               |
// |    uint64_t padding_value[4];   // a single Fr representing the padding value          |
// |    const uint64_t (*values)[4]; // the actual values of the column                     |
// |    uint64_t values_len;         // the length of the previous array                    |
// |  } ColumnData;                                                                         |
// +----------------------------------------------------------------------------------------+

func getColumnNames(trace *C.Trace) (r []string) {
	ncols := C.trace_column_count(trace)
	colnames_c := unsafe.Slice(C.trace_column_names(trace), ncols)

	for _, name_c := range colnames_c {
		name := C.GoString(name_c)
		r = append(r, name)
		fmt.Println(name)
	}
	fmt.Println(ncols, "columns found")

	return
}

// go:embed zkevm.bin
var zkevm string

func main() {
	// Load a .json(.gz), a constraint set, and return the computed & expanded trace
	corset, err := C.corset_from_file(C.CString("/home/franklin/consensys/zkevm/zk-geth/zk-evm/zkevm.bin"))
	// corset, err := C.corset_from_string(C.CString(zkevm))
	trace, err := C.trace_compute(
		corset,
		C.CString("/home/franklin/traces/1846717126.json"), // trace file
		4,     // # threads
		false, // crash on missing columns in the trace
	)
	if err != nil {
		fmt.Println("Error while computing trace")
		return
	}

	// Get all the column names
	names := getColumnNames(trace)
	fmt.Println("found columns:", names)

	// Take a look at a column in particular
	col, err := C.trace_column_by_name(trace, C.CString("rom__INV_rom_CODE_FRAGMENT_INDEX"))
	if err != nil {
		fmt.Println("while retrieving column")
		return
	}
	// Show padding value, values, and values length
	fmt.Println("col =", col)
	// Convert the raw pointer to a slice
	values := unsafe.Slice(col.values, col.values_len)
	// See what's in it
	fmt.Println(len(values), "values found")
	for i, op := range values {
		fmt.Println(i, op)
	}

	// Free memory
	C.trace_free(trace)
}
