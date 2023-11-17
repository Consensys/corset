/*
 * Copyright ConsenSys Inc.
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

import java.math.BigInteger;
import java.nio.MappedByteBuffer;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;
import net.consensys.linea.zktracer.ColumnHeader;
import net.consensys.linea.zktracer.types.UnsignedByte;
import org.apache.tuweni.units.bigints.UInt256;


/**
 * WARNING: This code is generated automatically.
 * Any modifications to this code may be overwritten and could lead to unexpected behavior.
 * Please DO NOT ATTEMPT TO MODIFY this code directly.
 */
public record Trace(
  {{#each registers}}
  @JsonProperty("{{ this.corset_name }}") List<{{ this.tupe }}> {{ this.java_name }}{{#unless @last}},{{/unless}}{{#if @last}}) { {{/if}}
  {{/each}}
  static TraceBuilder builder(int length) {
    return new TraceBuilder(length);
  }

  public int size() {
      return this.{{ registers.0.java_name }}.size();
  }

    public static List<ColumnHeader> headers(int size) {
    return List.of(
        {{#each registers}}
        new ColumnHeader("{{ this.corset_name }}", {{ this.bytes_width }}, size){{ #unless @last}},{{/unless}}
        {{/each}}
        );
    }



  static class TraceBuilder {
    private final BitSet filled = new BitSet();
    private int currentLine = 0;

    {{#each registers}}
    private MappedByteBuffer {{ this.java_name}};
    {{/each}}

    private TraceBuilder(int length) {
    }

    public int size() {
      if (!filled.isEmpty()) {
        throw new RuntimeException("Cannot measure a trace with a non-validated row.");
      }

      return this.currentLine;
    }

    public void setBuffers(List<MappedByteBuffer> buffers) {
        {{ #each registers }}
        this.{{ java_name }} = buffers.get({{ @index }});
        {{ /each }}
    }

    public void releaseBuffers() {
        {{ #each registers }}
        this.{{ java_name }} = null;
        {{ /each }}
    }

    {{#each columns}}
    public TraceBuilder {{ this.appender }}(final {{ this.tupe }} b) {
      if (filled.get({{ this.reg_id }})) {
        throw new IllegalStateException("{{ this.corset_name }} already set");
      } else {
        filled.set({{ this.reg_id }});
      }

      {{ this.register }}.{{ this.putter }};

      return this;
    }

    {{/each}}
    public TraceBuilder validateRow() {
      {{#each registers}}
      if (!filled.get({{ this.id }})) {
        throw new IllegalStateException("{{ this.corset_name }} has not been filled");
      }

      {{/each}}
      filled.clear();
      this.currentLine++;

      return this;
    }

    public TraceBuilder fillAndValidateRow() {
      filled.clear();
      this.currentLine++;

      return this;
    }

    public Trace build() {
      if (!filled.isEmpty()) {
        throw new IllegalStateException("Cannot build trace with a non-validated row.");
      }
      return null;
    }
  }
}
