# Dooc's Named Parameters

A header-only library containing string-indexed tuples (`named_tuple`).
With some help of C++20 concepts and user defined non-type template parameters we also have a decent named
argument API framework.

Tested with MSVC 2019.11.10 and GCC 10.3 (on Ubuntu).
Does not work with Clang compiler for now (seems to miss the deduction in user defined non-type template arguments
needed for the `template_string` to work.)

## Install
The library is header-only. The easiest way to use it is to download it and integrate it into a CMake project by
doing `add_subdirectory(path/to/lib [internal/build/path OPTIONAL])` and then use `target_link_libraries(<your target>
<PUBLIC/PRIVATE> dooc::named_parameters)`

### Requirements
- C++ 20
- MSVC 2019-11.10 (or newer) or GCC-10.3 (or newer). Clang currently does not work.

## Named argument

Named arguments are a feature in many programming languages that can improve readability and ease of use for APIs
that may have a lot of options. Python for example aalow the following syntax:
```python
def foo(arg1, arg2):
   ...

foo(arg2=5, arg1='hello!`)
```

This library intends to give C++ a similar capability without runtime overhead.

It is based around c++20:s concept. We can define a function that can use the named arguments like this:

```c++
// concept 'arg_with_any_name' means that TArgs all should be a named_arg_t or similar.
template<arg_with_any_name... TArgs>
  // args_fullfill evaluates that TArgs... have a variable named "min" that can implicitly convert to int
  //  and a variable called "max" under the same constraint.
  // arg_list is needed to differentiate between the required args and the supplied ones.
  requires args_fullfill<arg_list<named_type<int, "min">, named_type<int, "max">>, TArgs...>
void foo(TArgs const&... args) {
    // Retrieve the variables.
    do_something(get<"min">(args...));
    do_something_else(get<"max">(args...));
}
```

or (initial way)
```c++

// concept 'arg_with_any_name' means that TArgs all should be a named_arg_t or similar.
template<arg_with_any_name... TArgs>
// 'are_exactly_args' is a constexpr bool that is only true when the names in the template_string_list_t
//  and TArgs have a one-on-one (unordered) mapping.
  requires are_exactly_args<template_string_list_t<"min", "max">, TArgs...>
void foo(TArgs const&... args) {
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

Note that the order of parameters are unimportant.

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
Notice: the transform has a rather implicit 'ownership' behaviour (r-values becomes copies and l-values
become references...). This might change in the future (a possible alternative is to use the
`std::unwrap_ref_decay_t`-behaviour like the rest of the tuple to control whether a copy should be made
or not).

Lazy tuple-transform view. This creates a view of a tuple (`std::tuple` or `named_tuple`) and changes the
`get<...>`-function to apply a function on the value at the index before returning it. (An R-value ref
will become a copy instead in the transform.)
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

## Contribution:
I happily accept contributions and feature requests. If you add a pull request however it is strongly encouraged that
a test that shows the functional change is included in the pr (and it should fail without the rest of the changes).
Exceptions are fixes for a specific compiler and pure refactoring/documentation.


## Future plans:
- Make it work for Clang.
- Make a first release.
- Do arguments with in/out/inout/fwd/move
- Set up CI testing for supported compilers.

