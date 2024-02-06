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

import java.nio.MappedByteBuffer;
import java.util.BitSet;
import java.util.List;

import net.consensys.linea.zktracer.ColumnHeader;
import net.consensys.linea.zktracer.types.UnsignedByte;
import org.apache.tuweni.bytes.Bytes;

/**
 * WARNING: This code is generated automatically.
 *
 * <p>Any modifications to this code may be overwritten and could lead to unexpected behavior.
 * Please DO NOT ATTEMPT TO MODIFY this code directly.
 */
public class Trace {
  {{#each constants}}
  public static final {{ this.tupe }} {{ this.name }} = {{ this.value }};
  {{/each}}

  private final BitSet filled = new BitSet();
  private int currentLine = 0;

  {{#each registers}}
  private final MappedByteBuffer {{ this.java_name}};
  {{/each}}

  static List<ColumnHeader> headers(int length) {
    return List.of(
        {{ #each registers }}
        new ColumnHeader("{{ this.corset_name }}", {{ this.bytes_width }}, length){{ #if @last }});{{ else }},{{ /if }}
        {{ /each }}
  }

  public Trace(List<MappedByteBuffer> buffers) {
    {{ #each registers }}
    this.{{ java_name }} = buffers.get({{ @index }});
    {{ /each }}
  }

  public int size() {
    if (!filled.isEmpty()) {
      throw new RuntimeException("Cannot measure a trace with a non-validated row.");
    }

    return this.currentLine;
  }

  {{#each columns}}
  public Trace {{ this.appender }}(final {{ this.tupe }} b) {
    if (filled.get({{ this.reg_id }})) {
      throw new IllegalStateException("{{ this.corset_name }} already set");
    } else {
      filled.set({{ this.reg_id }});
    }

    {{ this.putter }}

    return this;
  }

  {{/each}}
  public Trace validateRow() {
    {{#each registers}}
    if (!filled.get({{ this.id }})) {
      throw new IllegalStateException("{{ this.corset_name }} has not been filled");
    }

    {{/each}}
    filled.clear();
    this.currentLine++;

    return this;
  }

  public Trace fillAndValidateRow() {
    {{#each registers}}
    if (!filled.get({{ this.id }})) {
      {{ this.java_name }}.position({{ this.java_name }}.position() + {{ this.bytes_width }});
    }

    {{/each}}
    filled.clear();
    this.currentLine++;

    return this;
  }

  public void build() {
    if (!filled.isEmpty()) {
      throw new IllegalStateException("Cannot build trace with a non-validated row.");
    }
  }
}
