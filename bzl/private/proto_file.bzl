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

"""Actions for writing out proto files."""

load("@bazel_skylib//lib:paths.bzl", "paths")

proto_attrs = {
    # The protocol_compiler is used both for haskell_proto_library rules as
    # well as to encode the HaskellCompile.pb file in binary proto format.
    "_protocol_compiler": attr.label(
        executable = True,
        cfg = "host",
        # Use the prebuilt binary to reduce the number of dependent actions.
        # This is particularly useful for the hrepl test which builds everything
        # from scratch, without the aid of Forge.
        default = Label("@com_google_protobuf//:protoc"),
    ),
    "_rule_info_proto": attr.label(
        allow_single_file = True,
        # Similar to protocol_compiler, reduce the number of dependent actions
        # by depending on the proto file itself rather than the proto_library
        # rule.
        default = Label("//rule_info:rule_info.proto"),
    ),
}

def write_proto_file(ctx, output_name, proto_type, content):
    """Write an encoded .proto file.

    Writes a file with the text format encoding, and then runs "protoc"
    to convert it to the wire encoding.

    Using the wire encoding allows us use released versions of tools like
    "hrepl" at CLs with different versions of the Haskell rules (within reason).

    Args:
      ctx: The current rule context.
      output_name: The output filename.  The text-encoded file will be named
        {output_name}.txt, and the encoded file will be named {file_name}.pb.
      proto_type: The type of the proto (e.g. foo.Bar).  It must be defined
        in rule_info.proto.
      content: A struct with the contents of the text file.

    Returns:
      A File containing the encoded proto message, named {file_name}.pb.
    """
    proto_txt = ctx.actions.declare_file(output_name + ".txt")
    proto_pb = ctx.actions.declare_file(output_name + ".pb")
    ctx.actions.write(output = proto_txt, content = content.to_proto())

    protoc = ctx.executable._protocol_compiler
    rule_info_proto = ctx.file._rule_info_proto

    ctx.actions.run_shell(
        outputs = [proto_pb],
        inputs = depset([proto_txt, rule_info_proto, protoc]),
        command =
            "{protoc} {rule_info_proto} --encode {proto_type} < {proto_txt} > {proto_pb}"
                .format(
                protoc = protoc.path,
                proto_type = proto_type,
                proto_txt = proto_txt.path,
                proto_pb = proto_pb.path,
                rule_info_proto = rule_info_proto.path,
            ),
    )
    return proto_pb

def _capitalize_first_letter(c):
    """Capitalize the first letter of the input.

    Unlike the built-in capitalize() method, doesn't lower-case the other
    characters.  This helps mimic the behavior of proto-lens-protoc, which turns
    Foo/Bar/BAZ.proto into Foo/Bar/BAZ.hs (rather than Foo/Bar/Baz.hs).

    Args:
      c: A non-empty string word.

    Returns:
      The input with the first letter upper-cased.
    """
    return c[0].capitalize() + c[1:]

def _camel_case(comp):
    """Camel-case the input string, preserving any existing capital letters."""

    # Split on both "-" and "_", matching the behavior of proto-lens-protoc.
    return "".join([
        _capitalize_first_letter(c2)
        for c1 in comp.split("_")
        for c2 in c1.split("-")
    ])

def _proto_lens_output_files(path):
    """The output files from proto-lens-protoc when run on the given path."""
    path = path[:-len(".proto")]
    joined_path = "/".join([_camel_case(p) for p in path.split("/")])
    return ["Proto/" + joined_path + infix + ".hs" for infix in ["", "_Fields"]]

def generate_proto_files(ctx, proto_info):
    """Uses proto-lens to generate Haskell .hs files from .proto files

    A file "{package}/foo.proto" will be turned into "{package}/foo.pb.hs".

    Args:
      ctx: The current rule context.
      proto_info: A ProtoInfo provider.

    Returns:
      A list of Files which are Haskell .hs files.
    """
    src_prefix = ctx.label.package + "/"

    # Put files in a lower-case output directory.  That has two advantages:
    # it makes it easier to see which files are generated for which proto_library
    # rule; and it makes sure the filepaths obey the longest-suffix module name rule.
    output_dir = ctx.label.name + ".hs-proto/"

    # When possible, just use the rule name itself for better readability, since
    # usually it's lower-case.  If it's upper-case, add a prefix.
    if output_dir[0:1].isupper():
        output_dir = "gen_" + output_dir

    hs_files = []
    for src in proto_info.direct_sources:
        # As with the native rules, require the .proto file to be in the same Bazel
        # package as the proto_library rule.  This allows us to put the output .hs
        # file next to the input .proto file.
        # Unfortunately Skylark doesn't let us check the package of the file
        # directly, so instead we just look at its short_path and rely on the
        # proto_library rule itself to check for consistency.
        # We use the file's short_path rather than its dirname/basename in case
        # it's in a subdirectory; for example, if the proto_library rule is in
        # "foo/BUILD" but the .proto file is "foo/bar/baz.proto".
        if not src.short_path.startswith(src_prefix):
            fail("Mismatch between rule context " + str(ctx.label.package) +
                 " and source file " + src.short_path)
        if src.basename[-6:] != ".proto":
            fail("bad extension for proto file " + src)

        # Strip off the package and the following "/" from the short_path.
        relative_path = src.short_path
        hs_files += [
            ctx.actions.declare_file(output_dir + f)
            for f in _proto_lens_output_files(relative_path)
        ]

    # Run the protocol compiler to generate the Haskell source files.
    plugin = "protoc-gen-haskell=" + ctx.executable._protoc_gen_haskell.path
    out_root_path = paths.join(ctx.bin_dir.path, ctx.label.package, output_dir)
    ctx.actions.run(
        inputs = ([ctx.executable._protoc_gen_haskell] +
                  # Since proto-lens-protoc uses ghc-source-gen, it needs
                  # to be able to access some GHC support files.
                  ctx.files._ghc_settings +
                  proto_info.transitive_sources.to_list()),
        outputs = hs_files,
        # This program parses the .proto files and feeds their descriptors into
        # the plugin program (protoc-gen-haskell) to generate Haskell sources.
        executable = ctx.executable._protocol_compiler,
        arguments = (
            # TODO(judahjacobson): For safety, pass --disallow_services to the
            # protocol compiler based on whether the deps specify
            # has_services=1.
            [
                "--plugin=" + plugin,
                "--haskell_out=no-runtime:" + out_root_path,
            ] +
            [
                "-I" + s.short_path + "=" + s.path
                for s in proto_info.transitive_sources.to_list()
            ] +
            [s.short_path for s in proto_info.direct_sources]
        ),
        progress_message = ("Generating Haskell proto-lens source files for %s" %
                            str(ctx.label)),
        mnemonic = "HaskellProtoLensGen",
    )
    return hs_files
