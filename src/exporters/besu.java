/*
 * Copyright ConsenSys AG.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package net.consensys.zktracer.module.{{ module }};

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import net.consensys.zktracer.bytes.UnsignedByte;

@JsonPropertyOrder({"Trace", "Stamp"})
@SuppressWarnings("unused")
public record {{ module_prefix }}Trace(@JsonProperty("Trace") Trace trace, @JsonProperty("Stamp") int stamp) {
    {{#each constants}}
    public static final BigInteger {{ this.name }} = BigInteger("{{ this.value }}");
    {{/each}}

    @JsonPropertyOrder({
    {{#each columns}}
        "{{ this.corset_name }}",
    {{/each}}
    })

    @SuppressWarnings("unused")
    public record Trace(
    {{#each columns}}
        @JsonProperty("{{ this.corset_name }}") List<{{ this.tupe }}> {{ this.corset_name }},
    {{/each}}
    )

    public static class Builder {
    {{#each columns}}
        private final List<{{ this.tupe }}> {{ this.java_name }} = new ArrayList<>();
    {{/each}}
    }

    private Builder() {}

    public Builder {
    {{#each columns}}
        {{ this.appender_name }}(final {{ this.tupe }} b) {
            {{ this.java_name }}.add(b);
            return this;
        }
    {{/each}}
    }

    public {{ module_prefix }}Trace build() {
        return new {{ module_prefix }}Trace(
            new Trace(
            {{#each columns}}
                {{ this.java_name }},
            {{/each}}
            )
        );
    }
}
