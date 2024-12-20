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

import net.consensys.linea.zktracer.ColumnHeader;
import net.consensys.linea.zktracer.types.UnsignedByte;
import org.apache.tuweni.bytes.Bytes;

/**
 * WARNING: This code is generated automatically.
 *
 * <p>Any modifications to this code may be overwritten and could lead to unexpected behavior.
 * Please DO NOT ATTEMPT TO MODIFY this code directly.
 */
public class {{ this.class }} {
  {{#each constants}}
  public static final {{ this.tupe }} {{ this.name }} = {{ this.value }};
  {{/each}}

  private final BitSet filled = new BitSet();
  private int currentLine = 0;

  {{#each registers}}
  private final MappedByteBuffer {{ this.java_name}};
  {{/each}}

  static List<ColumnHeader> headers(int length) {
      List<ColumnHeader> headers = new ArrayList<>();
      {{ #each registers }}
      headers.add(new ColumnHeader("{{ this.corset_name }}", {{ this.bytes_width }}, length));
      {{ /each }}
      return headers;
  }

  public {{ this.class }} (List<MappedByteBuffer> buffers) {
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
  public {{ this.class }} {{ this.appender }}(final {{ this.tupe }} b) {
    if (filled.get({{ this.reg_id }})) {
      throw new IllegalStateException("{{ this.corset_name }} already set");
    } else {
      filled.set({{ this.reg_id }});
    }

    {{ this.putter }}

    return this;
  }

  {{/each}}
  public {{ this.class }} validateRow() {
    {{#each registers}}
    if (!filled.get({{ this.id }})) {
      throw new IllegalStateException("{{ this.corset_name }} has not been filled");
    }

    {{/each}}
    filled.clear();
    this.currentLine++;

    return this;
  }

  public {{ this.class }} fillAndValidateRow() {
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
