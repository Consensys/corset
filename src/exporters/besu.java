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

package net.consensys.linea.zktracer.module.{{ module }};
import net.consensys.linea.zktracer.bytes.UnsignedByte;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.lang.IllegalStateException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

@SuppressWarnings("unused")
public record {{ module_prefix }}Trace(@JsonProperty("Trace") Trace trace) {
  {{#each constants}}
  public static final BigInteger {{ this.name }} = new BigInteger("{{ this.value }}");
  {{/each}}

  @SuppressWarnings("unused")
  private record Trace(
  {{#each registers}}
    @JsonProperty("{{ this.corset_name }}") List<{{ this.tupe }}> {{ this.java_name }}{{#unless @last}},{{/unless}}
  {{/each}}
  ) {}

  public static class Builder {
    {{#each registers}}
    private final List<{{ this.tupe }}> {{ this.java_name }} = new ArrayList<>();
    {{/each}}

    private final BitSet filled = new BitSet();

    public Builder() {}

    {{#each columns}}
    public Builder {{ this.appender }}(final {{ this.tupe }} b) {
      if (this.filled.get({{ this.reg_id }})) {
        throw new IllegalStateException("Can not append to {{ this.java_name }} before committing previous row");
      } else {
        this.filled.set({{ this.reg_id }});
      }
      {{ this.register }}.add(b);
      return this;
    }
    {{/each}}

    public void commit() {
      {{#each registers}}
      if (!this.filled.get({{ this.id }})) {
        throw new IllegalStateException("Can not commit row without setting value for {{ this.corset_name }}");
      }
      {{/each}}
      this.filled.clear();
    }

    public {{ module_prefix }}Trace build() {
      return new {{ module_prefix }}Trace(
        new Trace(
        {{#each registers}}
          {{ this.java_name }}{{#unless @last}},{{/unless}}{{#if @last}}){{/if}}
        {{/each}}
        );
    }
  }
}
