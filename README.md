# Dooc-NP
Dooc Named Parameters.

A header-only library containing string-indexed tuples (`named_tuple`).
With some help of c++20 concepts and user defined non-type template parameters we also have a decent named
argument API framework.

Currently been tested with MSVC 2019.11.10.


## Named argument

The more 'elegant' usage of the named arguments in the library would be to define your function like:

```c++

// concept 'arg_with_any_name' means that TArgs all should be a named_arg_t or similar.
template<arg_with_any_name... TArgs>
// 'are_exactly_args' is a constexpr bool that is only true when the names in the template_string_list_t
//  and Ts have a one-on-one (unordered) mapping.
  requires are_exactly_args<template_string_list_t<"min", "max">, Ts...>
void foo(Ts const&... args) {
    // Retrieve the variables.
    do_something(get<"min">(args...));
    do_something_else(get<"max">(args...));
}
```

And then when using the code:
```c++
foo(named_arg<"max">(5.), named_arg<"min">(0.1));

// or, more explicit
foo(named_arg_t<"max", double>(5.), named_arg_t<"min", double>(0.1));

// Or, for a more compact approach:
using namespace dooc::tuple_literals;
foo("max"_na = 5., "min"_na = 0.1);
```

If you need to explicitly state the type that goes into the function, you can declare it like:
```c++
void bar(named_tuple<named_arg_t<"min", double>, named_arg_t<"max", double>> const& args)
{
    do_something(get<"min">(args));
    // or
    using namespace dooc::tuple_literals;
    do_something("max"_from(args));
}
```

This leads to the user code needing to wrap their arguments in `{}`-brackets:
```c++
using namespace dooc::tuple_literals;
bar({"min"_na = 1., "max"_na = 36.});
```

A concept for this should be made (as to streamline the calling convention), but it is still a work in
progress.

## tuple slice view and transform:

More utilities in the library are the `tuple_slice` and `tuple_transform`. There is also an extended
version of `apply` tailored for the `named_tuple`-type.

### slice
The slice view allows one to take a large `named_tuple`, make a view of it and only expose a subset of
all the members inside it. It only works on `named_tuple`, not `std::tuple`.

For example:

```c++
using namespace dooc::tuple_literals;
using namespace std::string_literals;
named_tuple t1{"arg1"_na = 1., "arg2"_na = "my string"s, "arg3"_na = 4};
auto v1 = get_slice_view<"arg1", "arg2">(t1);
assert(get<"arg1">(v1) == 1.);
assert(get<"arg2">(v1).size() != 0);
// get<"arg3">(t1);  <- gives a compile error.
```

Can be useful to filter out things that you don't want to expose in an intricate API call somewhere.

### transform
Lazy tuple-transform view. This creates a view of a tuple (`std::tuple` or `named_tuple`) and changes the
`get<...>`-function to apply a function on the value at the index before returning it.
Example:

```c++
using namespace dooc::tuple_literals;
named_tuple t1{"double_arg"_na = 1., "int_arg"_na = 4};
auto transformed = transform([] (auto v) { return v * 2;}, t1);
assert(get<"double_arg">(transformed) == (1. * 2));
assert(get<"int_arg">(transformed) == (4 * 2));
```

You can chain-call transform and slice, making it useful when only a handful of parameters inside the
`named_tuple` are qualified for the transformation function.

### extended apply
`apply` has been extended for the `named_tuple`-type: you can supply a set of `template_string`:s
inside a `template_string_list_t` (last regular argument) to modify the order
of appliance. For example:
```c++
void foo(int a, std::string const& b);

// Don't use this readme for good example of names...
void bar(named_tuple<named_arg_t<"string", std::string>, named_arg_t<"int", int>> const& np) {
    // Notice the reversed order of args here. But we will still call 'foo' with the correct order
    //  of arguments.
    apply([] (auto const&... args) {foo(args...)}, np, template_string_list_t<"int", "string">{});
}
```


## Future plans:
- Add concept that makes explicit typed functions with named arguments same API-wise as the implicit typed.
- Test and make it work for GCC and Clang.
- Make a first release.


