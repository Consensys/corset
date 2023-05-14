package net.consensys.linea.jsonrpc

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonIgnoreProperties

val MODULES = listOf(
{{ #each modules }}
  Pair("{{ this.original_name }}", {{ this.klass_name }}::class.java),
{{ /each }}
)
{{ #each modules }}

@JsonIgnoreProperties(ignoreUnknown = true)
data class {{ this.klass_name }}(
  {{ #each this.columns }}
  @JsonProperty("{{ this }}") val {{ this }}: MutableList<String> = arrayListOf(),
  {{ /each }}
)
{{ /each }}

open class ConflatedTraceStorage (
{{ #each modules }}
  var {{ this.member_name }}: {{ this.klass_name}} = {{ this.klass_name }}(),
{{ /each }}
)
