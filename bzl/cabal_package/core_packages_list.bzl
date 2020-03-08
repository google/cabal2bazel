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

"""A build rule for exposing the list of core packages to Cabal2Build."""

load("//bzl/private:default_packages.bzl", "core_package_names")

def _impl(ctx):
    output = ctx.actions.declare_file(ctx.label.name)
    ctx.actions.write(
        output = output,
        content = "\n".join(core_package_names),
    )
    return [DefaultInfo(files = depset([output]))]

core_packages_list = rule(
    implementation = _impl,
)
