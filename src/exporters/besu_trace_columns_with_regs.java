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
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

@SuppressWarnings("unused")
class Trace {
  {{#each constants}}
  public static final BigInteger {{ this.name }} = new BigInteger("{{ this.value }}");
  {{/each}}
  final BitSet filled = new BitSet();

  {{#each registers}}
  @JsonProperty("{{ this.corset_name }}") List<{{ this.tupe }}> {{ this.java_name }} = new ArrayList<>();
  {{/each}}

  {{#each columns}}
  public Trace {{ this.appender }}(final {{ this.tupe }} b) {
    if (filled.get({{ this.reg_id }})) {
       throw new IllegalStateException("{{ this.corset_name }} already set");
    } else {
       filled.set({{ this.reg_id }});
    }
    {{ this.register }}.add(b);
    return this;
  }
  {{/each}}

  public void commitRow() {
    {{#each registers}}
    if (!filled.get({{ this.id }})) {
      throw new IllegalStateException("{{ this.corset_name }} has not been filled");
    }
    {{/each}}
  }
}
