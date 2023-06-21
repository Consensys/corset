package net.consensys.linea.traces

import com.fasterxml.jackson.annotation.JsonAutoDetect
import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.fasterxml.jackson.annotation.JsonProperty
import io.vertx.core.json.JsonObject
import kotlin.Pair

fun JsonObject.getTrace(jsonPath: List<String>): JsonObject? {
  var jsonObject: JsonObject? = this
  for (node in jsonPath) {
    jsonObject = jsonObject?.getJsonObject(node)
  }
  return jsonObject?.getJsonObject("Trace")
}

val MODULES = listOf(
{{ #each modules }}
  Pair(listOf("{{ this.original_name }}"), {{ this.klass_name }}::class.java),
{{ /each }}
)
{{ #each modules }}

@JsonIgnoreProperties(ignoreUnknown = true)
data class {{ this.klass_name }}(
  {{ #each this.columns }}
  @get:JsonProperty("{{ this.json_name }}")
  val {{ this.safe_name }}: MutableList<String> = arrayListOf(),
  {{ /each }}
)
{{ /each }}

open class ConflatedTraceStorage (
{{ #each modules }}
  var {{ this.member_name }}: {{ this.klass_name}} = {{ this.klass_name }}(),
{{ /each }}
)
