package {{ module }}

import (
	"github.com/ethereum/go-ethereum/zk-evm/zeroknowledge/witnessdata/column"
)

const (
{{#each constants}}
	{{this.name}} = {{this.value}}
{{/each}}
)


var (
	{{#each columns}}
	{{this.go_name}} column.ColumnID = column.ColumnID{ Id: {{this.reg_id}}, Str: "{{this.reg_name}}" }
	{{/each}}
)

var AllRegisters = column.ColumnList{
	{{#each registers}}
	{{this.1}},
	{{/each}}
}
