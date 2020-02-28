/* Common defines for HeadersTest and HeadersLib.
 * Note: lint wants us to put "#ifndef..." guards on C++ header (*.h files),
 * including a trailing "//".
 * Unfortunately, that badly breaks the compile chain, in part because
 * this file will be spliced into Haskell code instead of C++ code.
 * Normally we'd just add a "// NOLINT" pragma but, for the same reason, we
 * can't use "//" comments here.
 */
#define FOO 42
#define BAR 17
