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
public record Trace(
  {{#each registers}}
  @JsonProperty("{{ this.corset_name }}") List<{{ this.tupe }}> {{ this.java_name }}{{#unless @last}},{{/unless}}{{#if @last}}) { {{/if}}
  {{/each}}
  static TraceBuilder builder() {
    return new TraceBuilder();
  }

  public int size() {
      return this.{{ registers.0.java_name }}.size();
  }

  static class TraceBuilder {
    private final BitSet filled = new BitSet();

    {{#each registers}}
    @JsonProperty("{{ this.corset_name }}")
    private final List<{{ this.tupe }}> {{ this.java_name }} = new ArrayList<>();
    {{/each}}

    private TraceBuilder() {}

    public int size() {
      if (!filled.isEmpty()) {
        throw new RuntimeException("Cannot measure a trace with a non-validated row.");
      }

      return this.{{ registers.0.java_name }}.size();
    }

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

    public TraceBuilder validateRow() {
      {{#each registers}}
      if (!filled.get({{ this.id }})) {
        throw new IllegalStateException("{{ this.corset_name }} has not been filled");
      }

      {{/each}}

      filled.clear();

      return this;
    }

    public TraceBuilder fillAndValidateRow() {
      {{#each registers}}
      if (!filled.get({{ this.id }})) {
          {{ this.java_name }}.add({{ this.zero_value }});
          this.filled.set({{ this.id }});
      }
      {{/each}}

      return this.validateRow();
    }

    public Trace build() {
      if (!filled.isEmpty()) {
        throw new IllegalStateException("Cannot build trace with a non-validated row.");
      }

      return new Trace(
        {{#each registers}}
        {{ this.java_name }}{{#unless @last}},{{/unless}}{{#if @last}});{{/if}}
        {{/each}}
    }
  }
}
