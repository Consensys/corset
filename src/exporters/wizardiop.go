package define

import (
	_ "embed"

	// "github.com/BurntSushi/toml"
	"github.com/consensys/accelerated-crypto-monorepo/symbolic"
	"github.com/consensys/accelerated-crypto-monorepo/zkevm"
)


// type traces_limits struct {
// 	ADD                 int
// 	BIN                 int
// 	BIN_RT              int
// 	EC_DATA             int
// 	EXT                 int
// 	HASH_DATA           int `toml:"PUB_HASH"`
// 	HASH_INFO           int `toml:"PUB_HASH_INFO"`
// 	HUB                 int
// 	INSTRUCTION_DECODER int
// 	LOG_DATA            int `toml:"PUB_LOG"`
// 	LOG_INFO            int `toml:"PUB_LOG_INFO"`
// 	MMIO                int
// 	MMU                 int
// 	MOD                 int
// 	MUL                 int
// 	MXP                 int
// 	PHONEY_RLP          int
// 	RLP                 int
// 	ROM                 int
// 	SHF                 int
// 	SHF_RT              int
// 	TX_RLP              int
// 	WCP                 int
// }

// func fromToml(input string) (sizes traces_limits) {
// 	var r map[string]traces_limits
// 	_, err := toml.Decode(input, &r)
// 	if err != nil {
// 		panic(err)
// 	}
// 	return r["traces-limits"]
// }

// TODO: __go:embed traces-limits-v1.toml
// TODO: var limitStr string

func ZkEVMDefine(build *zkevm.Builder) {
	// TODO: sizes := fromToml(limitStr)
	
	//
	// Corset-computed columns
	//
	{{ #each columns }}
	{{ go_id }} := build.RegisterCommit("{{ json_register }}", {{ size }})
	{{ /each }}


	//
	// Interleaved columns
	//
	{{ #each interleaved }}
	{{ go_id }} := zkevm.Interleave({{ interleaving }})
	{{ /each }}


	//
	// Constraints
	//
	{{ #each constraints }}
	{{{ this }}}
	{{ /each }}	
}
