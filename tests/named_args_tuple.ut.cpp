//          Copyright Robin Söderholm 2021 - 2022.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          https://www.boost.org/LICENSE_1_0.txt)

#include <dooc/named_args_tuple.hpp>

#include <string_view>

#include <gmock/gmock.h>

namespace dooc {
using namespace ::testing;
using namespace tuple_literals;
using namespace std::string_view_literals;

template <typename...> constexpr bool function_compiles = false;
template <typename TFunc, typename... Ts>
  requires requires(TFunc f, Ts... ts) { f(ts...); }
constexpr bool function_compiles<TFunc, Ts...> = true;

template <typename... Ts>
constexpr bool function_compiles_f(Ts &&...) noexcept {
  return function_compiles<Ts...>;
}

static_assert(function_compiles_f([]() {}));
static_assert(function_compiles_f([](int) {}, 1));
static_assert(!function_compiles_f([](int) {}, "not an int"));

TEST(NamedTuple, Creation) // NOLINT
{
  auto a2 = "hi there!"sv;
  auto t1 = make_named_args(named_arg<"arg1">(2), named_arg<"arg_2">(a2));
  int &v1 = get<"arg1">(t1);
  std::string_view &v2 = get<"arg_2">(t1);
  EXPECT_THAT(v2, StrEq("hi there!"sv));
  EXPECT_THAT(v1, Eq(2));
}

TEST(NamedTuple, CreationUsingLiterals) // NOLINT
{
  auto t1 = make_named_args("arg1"_na(2), "arg_2"_na("hi there!"sv));
  int &v1 = get<"arg1">(t1);
  std::string_view &v2 = "arg_2"_from(t1);
  EXPECT_THAT(v2, StrEq("hi there!"sv));
  EXPECT_THAT(v1, Eq(2));
}

TEST(NamedTuple, GetOnRValue) // NOLINT
{
  EXPECT_THAT(get<"arg1">(named_tuple("arg1"_na(2))), Eq(2));
}

TEST(NamedTuple, TupleCat) // NOLINT
{
  auto t1 = make_named_args("arg1"_na(2), "arg_2"_na("hi there!"sv));
  named_tuple t2 = {"arg3"_na(2.3), "super duper"_na(55)};
  auto t3 = named_tuple_cat(t1, t2);
  EXPECT_THAT(get<"arg1">(t3), Eq(2));
  EXPECT_THAT(get<"super duper">(t3), Eq(55));
  auto t4 = named_tuple_cat(t1, t2, named_tuple("just another arg"_na(33.)));
  EXPECT_THAT(get<"just another arg">(t4), Eq(33.));
}

TEST(NamedTuple, ElementExists) // NOLINT
{
  named_tuple t1 = ("arg1"_na(2));
  static_assert(contains_arg<"arg1">(t1));
  static_assert(!contains_arg<"arg2">(t1));
}

TEST(NamedTuple, GetSlice) // NOLINT
{
  named_tuple t1("a1"_na(1), "a2"_na(2), "a3"_na = 44);
  auto t2 = get_slice_view<"a1", "a2">(t1);
  static_assert(contains_arg<"a1">(t2));
  static_assert(contains_arg<"a2">(t2));
  static_assert(!contains_arg<"a3">(t2));
  static_assert(std::tuple_size<decltype(t2)>::value == 2);
  EXPECT_THAT(get<"a1">(t2), Eq(1));
  EXPECT_THAT(get<"a2">(t2), Eq(2));
}

template <typename, typename>
constexpr bool test_that_function_is_allowed = false;

template <typename TTuple, details::func_works_with_tuple_c<TTuple> TFunc>
constexpr bool test_that_function_is_allowed<TTuple, TFunc> = true;

TEST(NamedTuple, ChainSpliceTransform) // NOLINT
{
  named_tuple t1("a1"_na(1), "a2"_na(2), "a3"_na = 44);
  auto t2 = transform([](auto const &a, auto const &) { return a * 3; },
                      get_slice_view<"a1", "a2">(t1));

  constexpr auto f = [](auto const &) { return 1; };
  constexpr auto f2 = [](auto const &, auto &&) {};
  constexpr auto f3 = [](auto const &, auto &&) { return 1; };
  static_assert(
      !test_that_function_is_allowed<std::remove_cvref_t<decltype(t1)>,
                                     std::remove_cvref_t<decltype(f)>>);
  static_assert(
      !test_that_function_is_allowed<std::remove_cvref_t<decltype(t1)>,
                                     std::remove_cvref_t<decltype(f2)>>);
  static_assert(
      test_that_function_is_allowed<std::remove_cvref_t<decltype(t1)>,
                                    std::remove_cvref_t<decltype(f3)>>);
  static_assert(contains_arg<"a1">(t2));
  static_assert(contains_arg<"a2">(t2));
  static_assert(!contains_arg<"a3">(t2));
  EXPECT_THAT(get<"a1">(t2), Eq(1 * 3));
  EXPECT_THAT(get<"a2">(t2), Eq(2 * 3));
  auto t3 = get_slice_view<"a1", "a2">(
      transform([](auto const &a, auto const &) { return a * 4; }, t1));
  static_assert(contains_arg<"a1">(t3));
  static_assert(contains_arg<"a2">(t3));
  static_assert(!contains_arg<"a3">(t3));
  EXPECT_THAT(get<"a1">(t3), Eq(1 * 4));
  EXPECT_THAT(get<"a2">(t3), Eq(2 * 4));
}

constexpr void foo(int &) {}

TEST(NamedTuple, DifferentOrderCopyAssign) // NOLINT
{
  using tuple_1_t =
      named_tuple<named_arg_t<"a1", int>, named_arg_t<"a2", double>,
                  named_arg_t<"a3", std::string_view>>;

  tuple_1_t t1{};
  named_tuple t2("a3"_na = "hi"sv, "a2"_na = 1., "a1"_na = 3);
  using tuple_2_t = std::remove_reference_t<decltype(t2)>;

  static_assert(std::is_same_v<named_tuple_element_t<"a1", tuple_1_t>, int>);
  static_assert(std::is_same_v<named_tuple_element_t<"a1", tuple_2_t>, int>);
  static_assert(std::is_same_v<named_tuple_element_t<"a2", tuple_1_t>, double>);
  static_assert(std::is_same_v<named_tuple_element_t<"a2", tuple_2_t>, double>);
  static_assert(
      std::is_same_v<named_tuple_element_t<"a3", tuple_1_t>, std::string_view>);
  static_assert(
      std::is_same_v<named_tuple_element_t<"a3", tuple_2_t>, std::string_view>);
  static_assert(std::is_same_v<named_tuple_element_t<"a3", const tuple_2_t>,
                               const std::string_view>);
  t1 = t2;
  tuple_1_t t3(t2);
  EXPECT_THAT(get<"a1">(t1), Eq(3));
  EXPECT_THAT(get<"a2">(t1), Eq(1.));
  EXPECT_THAT(get<"a3">(t1), StrEq("hi"sv));
  EXPECT_THAT(get<"a1">(t3), Eq(3));
}

TEST(NamedTuple, Apply) // NOLINT
{
  constexpr template_string_list_t<"a3", "a1", "a2"> arg_ord{};
  MockFunction<void(int, int, double)> f1;
  MockFunction<void(std::string_view, std::string_view, std::string_view)> f1t;
  MockFunction<void(double, int, int)> f2;
  MockFunction<void(std::string_view, std::string_view, std::string_view)> f2t;
  EXPECT_CALL(f1, Call(1, 2, 44.));
  EXPECT_CALL(f2, Call(44., 1, 2));
  named_tuple t1("a1"_na(1), "a2"_na(2), "a3"_na = 44.);
  apply([&f1](auto const &...args) { f1.Call(args...); }, t1);
  apply([&f2](auto const &...args) { f2.Call(args...); }, t1, arg_ord);
}

TEST(NamedTuple, SliceApply) // NOLINT
{
  MockFunction<void(int, int)> f1;
  EXPECT_CALL(f1, Call(1, 2));
  named_tuple t1("a1"_na(1), "a2"_na(2), "a3"_na = 44);
  auto t2 = get_slice_view<"a1", "a2">(t1);
  apply([&f1](auto const &...args) { f1.Call(args...); }, t2);
}

TEST(NamedTuple, TransformNamed) // NOLINT
{
  named_tuple t1("a1"_na = 1, "a2"_na = 1., "a3"_na = 3);
  auto t1t = transform(
      []<typename T>(T const &arg, auto const &) { return arg * 2; }, t1);
  EXPECT_THAT(get<"a1">(t1t), Eq(1 * 2));
  EXPECT_THAT(get<"a2">(t1t), Eq(1. * 2));
  EXPECT_THAT(get<"a3">(t1t), Eq(3 * 2));

  named_tuple t2("a1"_na = "asdas");
  auto t2t = transform(
      [](std::string arg, std::string_view name) { return arg.append(name); },
      t2);
  EXPECT_THAT(get<"a1">(t2t), StrEq("asdasa1"));
}

TEST(NamedArg, GetFromBunch) // NOLINT
{
  named_arg_t arg1 = named_arg<"arg1">(3.);
  named_arg_t arg2 = named_arg<"arg2">(91.);

  EXPECT_THAT(get<"arg1">(arg1, arg2), Eq(3.));
  EXPECT_THAT(get<"arg2">(arg1, arg2), Eq(91.));

  EXPECT_THAT("arg1"_from(arg1, arg2), Eq(arg1));
}

TEST(NamedArg, FullCoverage) // NOLINT
{
  named_arg_t arg1 = named_arg<"arg1">(3.);
  named_arg_t arg2 = named_arg<"arg2">(91.);

  static_assert(covers_args<template_string_list_t<"arg1", "arg2">,
                            decltype(arg1), decltype(arg2)>);
  static_assert(covers_args<template_string_list_t<"arg2", "arg1">,
                            decltype(arg1), decltype(arg2)>);
  static_assert(are_exactly_args<template_string_list_t<"arg1", "arg2">,
                                 decltype(arg1), decltype(arg2)>);
  static_assert(are_exactly_args<template_string_list_t<"arg2", "arg1">,
                                 decltype(arg1), decltype(arg2)>);
  static_assert(covers_args<template_string_list_t<"arg2">, decltype(arg1),
                            decltype(arg2)>);
  static_assert(!are_exactly_args<template_string_list_t<"arg2">,
                                  decltype(arg1), decltype(arg2)>);
  static_assert(!covers_args<template_string_list_t<"arg2", "arg3">,
                             decltype(arg1), decltype(arg2)>);
  static_assert(!covers_args<template_string_list_t<"arg1", "arg2", "arg3">,
                             decltype(arg1), decltype(arg2)>);
}

TEST(NamedArg, TypedFullCoverage) // NOLINT
{
  constexpr auto f = []<arg_with_any_name... Ts>
    requires args_fullfill<
        arg_list<named_type<int, "arg1">, named_type<std::string_view, "arg2">>,
        Ts...>(Ts const &...) {};
  static_assert(
      arg_list<named_type<int, "arg1">, named_type<std::string_view, "arg2">>::
          template fullfilled_by<named_arg_t<"arg1", int>,
                                 named_arg_t<"arg2", std::string_view>>);
  EXPECT_TRUE(function_compiles_f(f, "arg1"_na = 1, "arg2"_na = "hi!"sv));
  EXPECT_TRUE(function_compiles_f(f, "arg1"_na = 1, "arg2"_na = "hi!"));
  EXPECT_FALSE(
      function_compiles_f(f, "arg1"_na = 1, "arg2"_na = "hi!", "arg3"_na = 2));
  EXPECT_FALSE(function_compiles_f(f, "arg1"_na = 1, "arg2"_na = 444));
  EXPECT_FALSE(function_compiles_f(f, "arg1"_na = 1));

  // This last one may be very hard to assert. Just doing a check on the number
  // of arguments would fix this particular error, but that would not work if we
  // start to mix in optional arguments as well...
  // EXPECT_FALSE(function_compiles_f(f, "arg1"_na = 1, "arg2"_na = "hi!",
  // "arg1"_na = 2));
}

} // namespace dooc
