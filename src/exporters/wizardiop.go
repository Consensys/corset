package define

import (
	_ "embed"

	"github.com/consensys/accelerated-crypto-monorepo/symbolic"
	"github.com/consensys/accelerated-crypto-monorepo/zkevm"
)

func ZkEVMDefine(build *zkevm.Builder) {
	
	
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
