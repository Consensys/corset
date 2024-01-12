package define

import (
	_ "embed"

	"github.com/consensys/zkevm-monorepo/prover/symbolic"
)

func ZkEVMDefine(build *Builder) {
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
	{{ go_id }} := build.Interleave({{ interleaving }})
	{{ /each }}


	//
	// Constraints
	//
	{{ #each constraints }}
	{{{ this }}}
	{{ /each }}	
}
