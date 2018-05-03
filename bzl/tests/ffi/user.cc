// Defined in provider.cc
int provider(int x);

extern "C" {
int user(int x) { return 3 * provider(x); }
};
