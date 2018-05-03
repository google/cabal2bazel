# Copyright 2018 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Utility functions to construct package metadata.
"""

def package_key(ctx, name_override = None):
  """Get a globally-unique GHC package name for a rule.

  Args:
    ctx: The context of a rule.
    name_override: If want to get the package key of another target in the same
                   package.
  Returns:
    A GHC package identifier, based on the Bazel package and the (overriden)
    name of this rule, which is unique within google3.  It escapes characters
    that are allowed by Bazel but not by GHC.  It also disambiguates between
    different proto API versions.
  """
  res = ctx.label.package + "/" + (name_override or ctx.label.name)
  # "-"s are technicaly allowed, but we escape them since digits can't appear
  # by themselves without a letter.  (Otherwise, ghc might try to treat them
  # like a version number, e.g., "abc-12").
  # (Put this first so it doesn't interact with the other replacements that add
  # "-"s.
  res = res.replace("-", "D-D")
  res = res.replace("/", "S-S")
  res = res.replace("_", "U-U")

  if hasattr(ctx.attr, "_api"):
    res += ctx.attr._api

  return res
