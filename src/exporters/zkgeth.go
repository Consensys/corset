package {{ module }}

import (
	"github.com/ethereum/go-ethereum/zk-evm/zeroknowledge/witnessdata/column"
)

const (
{{#each constants}}
	{{this.name}} = {{this.value}}
{{/each}}
)


const (
	{{#each columns}}
	{{this.go_name}} column.ColumnID = "{{this.corset_name}}"
	{{/each}}
)

var AllRegisters = column.ColumnList{
	{{#each registers}}
	"{{this}}",
	{{/each}}
}
