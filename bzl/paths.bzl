# Copyright 2020 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Utilities for handling filepaths."""

def split_dirname(dirname):
    """Splits the capitalized suffix out from the given directories.

    Looks for the longest suffix that is CamelCased;
    specifically, where each directory starts with a capitalized letter.

    See go/haskell-standard-names for the motivation of this scheme.

    For example:
        split_dirname("abc/Def/Ghi") == ("abc", "Def/Ghi")
        split_dirname("abc/def") == ("abc/def", "")

    Args:
      dirname: A string; a path of directories separated by "/".

    Returns:
      A pair of (prefix, suffix) strings.
    """
    dirs = dirname.split("/")

    for i in range(len(dirs) - 1, -1, -1):
        if not dirs[i][0:1].isupper():  # Make sure to exclude symbols
            return ("/".join(dirs[0:(i + 1)]), "/".join(dirs[(i + 1):]))

    # Every component is lower-cased:
    return (dirname, "")
