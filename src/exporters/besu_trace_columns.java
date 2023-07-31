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

import com.fasterxml.jackson.annotation.JsonProperty;
import net.consensys.linea.zktracer.bytes.UnsignedByte;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

/**
 * WARNING: This code is generated automatically.
 * Any modifications to this code may be overwritten and could lead to unexpected behavior.
 * Please DO NOT ATTEMPT TO MODIFY this code directly.
 */
record Trace(
  {{#each registers}}
  @JsonProperty("{{ this.corset_name }}") List<{{ this.tupe }}> {{ this.java_name }}{{#unless @last}},{{/unless}}{{#if @last}}) { {{/if}}
  {{/each}}
  {{#each constants}}
  public static final BigInteger {{ this.name }} = new BigInteger("{{ this.value }}");
  {{/each}}

  public static TraceBuilder builder() {
    return new TraceBuilder();
  }

  public static class TraceBuilder {
    private final BitSet filled = new BitSet();

    {{#each registers}}
    private final List<{{ this.tupe }}> {{ this.java_name }} = new ArrayList<>();
    {{/each}}

    private TraceBuilder() {}

    {{#each columns}}
    public TraceBuilder {{ this.appender }}(final {{ this.tupe }} b) {
      if (filled.get({{ this.reg_id }})) {
        throw new IllegalStateException("{{ this.corset_name }} already set");
      } else {
        filled.set({{ this.reg_id }});
      }

      {{ this.register }}.add(b);

      return this;
    }
    {{#unless @last}}

    {{/unless}}
    {{/each}}

    {{#each columns}}
    public TraceBuilder set{{ this.updater }}At(final {{ this.tupe }} b, int i) {
      {{ this.register }}.set(i, b);

      return this;
    }
    {{#unless @last}}

    {{/unless}}
    {{/each}}

    {{#each columns}}
    public TraceBuilder set{{ this.updater }}Relative(final {{ this.tupe }} b, int i) {
      {{ this.register }}.set({{ this.register }}.size() - 1 - i, b);

      return this;
    }
    {{#unless @last}}

    {{/unless}}
    {{/each}}

    public void validateRow() {
      {{#each registers}}
      if (!filled.get({{ this.id }})) {
        throw new IllegalStateException("{{ this.corset_name }} has not been filled");
      }
      {{#unless @last}}

      {{/unless}}
      {{/each}}
    }

    public Trace build() {
      validateRow();

      return new Trace(
        {{#each registers}}
        {{ this.java_name }}{{#unless @last}},{{/unless}}{{#if @last}});{{/if}}
        {{/each}}
    }
  }
}
