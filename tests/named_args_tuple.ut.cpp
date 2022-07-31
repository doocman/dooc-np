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
  auto a2_pre = "arg_2"_from;
  std::string_view &v2 = a2_pre(t1);
  // std::string_view &v2 = "arg_2"_from(t1); // This crashes clang-12 compiler
  // frontend for some reason...
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
  named_tuple t1("arg1"_na(2), "arg3"_na(55));
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
constexpr bool function_is_valid_tuple_transform = false;

template <typename TTuple, details::func_works_with_tuple_c<TTuple> TFunc>
constexpr bool function_is_valid_tuple_transform<TTuple, TFunc> = true;

TEST(NamedTuple, ChainSpliceTransform) // NOLINT
{
  named_tuple t1("a1"_na(1), "a2"_na(2), "a3"_na = 44);
  auto t2 = transform([](auto const &a, auto const &) { return a * 3; },
                      get_slice_view<"a1", "a2">(t1));

  constexpr auto f = [](auto const &) { return 1; };
  constexpr auto f2 = [](auto const &, auto &&) {};
  constexpr auto f3 = [](auto const &, auto &&) { return 1; };
  static_assert(
      !function_is_valid_tuple_transform<std::remove_cvref_t<decltype(t1)>,
                                         std::remove_cvref_t<decltype(f)>>);
  static_assert(
      !function_is_valid_tuple_transform<std::remove_cvref_t<decltype(t1)>,
                                         std::remove_cvref_t<decltype(f2)>>);
  static_assert(
      function_is_valid_tuple_transform<std::remove_cvref_t<decltype(t1)>,
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

struct typed_full_coverage_impl {
  template <arg_with_any_name... Ts>
    requires(args_fullfill<arg_list<named_type<int, "arg1">,
                                    named_type<std::string_view, "arg2">>,
                           Ts...>)
  void operator()(Ts const &...) const {}
};

TEST(NamedArg, TypedFullCoverage) // NOLINT
{
  constexpr typed_full_coverage_impl f;
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

struct optional_arg_impl {
  template <arg_with_any_name... Ts>
    requires(args_fullfill<arg_list<optional_typed_arg<int, "arg1">,
                                    optional_auto_arg<"arg2">>,
                           Ts...>)
  std::pair<bool, bool>
  operator()(Ts const &...) const {
    return {arg_provided<"arg1", Ts...>, arg_provided<"arg2", Ts...>};
  }
};

TEST(NamedArg, OptionalArgs) // NOLINT
{
  constexpr optional_arg_impl f;
  EXPECT_TRUE(function_compiles_f(f, "arg1"_na = 1));
  EXPECT_TRUE(function_compiles_f(f));
  EXPECT_THAT(get_or<"not here">(5, "arg1"_na = 1, "arg2"_na = "sads"), Eq(5));
  EXPECT_THAT(get_or<"arg1">(5, "arg1"_na = 1, "arg2"_na = "sads"), Eq(1));
  auto res = f("arg1"_na = 1);
  EXPECT_TRUE(res.first);
  EXPECT_FALSE(res.second);
}

TEST(NamedArg, ConstructContainer) // NOLINT
{
  std::vector<int> v1(construct(get_or<"arg1">(1, "arg1"_na = {1, 2, 3, 4})));
  EXPECT_THAT(v1, ElementsAre(1, 2, 3, 4));
  std::vector<int> v2(
      construct(get_or<"not here">(1, "arg1"_na = {1, 2, 3, 4})));
  EXPECT_THAT(v2, ElementsAre(0));
}

TEST(NamedArg, ConcatTupleWithReferences) // NOLINT
{
  int v1 = 0;
  int v2 = 0;
  auto v1_tuple = named_tuple(named_arg<"a1">(std::ref(v1)));
  auto v2_tuple = named_tuple(named_arg<"a2">(std::ref(v2)));
  auto concat_tuple = named_tuple_cat(v1_tuple, v2_tuple);
  EXPECT_THAT(get<"a1">(concat_tuple), Ref(v1));
  EXPECT_THAT(get<"a2">(concat_tuple), Ref(v2));
}

TEST(NamedArg, TupleCatView) // NOLINT
{
  auto v1_tuple = named_tuple(named_arg<"a1">(1));
  auto v2_tuple = named_tuple(named_arg<"a2">(2));
  auto concat_tuple = named_tuple_cat_view(v1_tuple, v2_tuple);
  static_assert(
      std::is_same_v<named_tuple_element_t<
                         "a1", std::remove_reference_t<decltype(concat_tuple)>>,
                     int>);
  static_assert(
      std::is_same_v<named_tuple_element_t<
                         "a2", std::remove_reference_t<decltype(concat_tuple)>>,
                     int>);
  EXPECT_THAT(get<"a1">(concat_tuple), Ref(get<"a1">(v1_tuple)));
  EXPECT_THAT(get<"a2">(concat_tuple), Ref(get<"a2">(v2_tuple)));
  auto sl = get_slice_view<"a1">(concat_tuple);
  EXPECT_THAT(get<"a1">(sl), Ref(get<"a1">(v1_tuple)));
}

TEST(NamedArg, CatViewOfTransform) // NOLINT
{
  auto v1_tuple = named_tuple(named_arg<"a1">(1));
  auto v2_tuple_raw = named_tuple(named_arg<"a2">(2));
  auto v2_tuple = transform([](auto const &v, auto const &) { return v + 1; },
                            v2_tuple_raw);
  auto v3_tuple_raw =
      named_tuple(named_arg<"a3">(3), named_arg<"not used">(677));
  auto v3_tuple = get_slice_view<"a3">(v3_tuple_raw);
  auto concat_tuple = named_tuple_cat_view(v1_tuple, v2_tuple, v3_tuple);
  EXPECT_THAT(get<"a2">(concat_tuple), Eq(get<"a2">(v2_tuple)));
  auto sliced = get_slice_view<"a1">(concat_tuple);
  EXPECT_THAT(get<"a1">(sliced), Ref(get<"a1">(v1_tuple)));
  EXPECT_THAT(get<"a3">(concat_tuple), Ref(get<"a3">(v3_tuple_raw)));

  static_assert(has_tags_list<std::remove_cvref_t<decltype(concat_tuple)>>);

  auto concat_transformed = transform(
      [](auto const &v, auto const &) { return v + 1; }, concat_tuple);
  EXPECT_THAT(get<"a2">(concat_transformed), Eq(get<"a2">(v2_tuple) + 1));
  EXPECT_THAT(get<"a1">(concat_transformed), Eq(get<"a1">(concat_tuple) + 1));
  EXPECT_THAT(get<"a3">(concat_transformed), Eq(get<"a3">(concat_tuple) + 1));
}

TEST(NamedArg, TupleForEach) // NOLINT
{
  constexpr auto v1_tuple = named_tuple("1"_na(1), "2"_na(2), "3"_na(3));
  std::vector<int> v_ints;
  std::vector<std::string> v_strings;
  tuple_for_each(
      [&v_ints, &v_strings](std::string_view name, int value) {
        v_ints.push_back(value);
        v_strings.emplace_back(name);
      },
      v1_tuple);
  EXPECT_THAT(v_ints, ElementsAre(1, 2, 3));
  EXPECT_THAT(v_strings, ElementsAre(StrEq("1"), StrEq("2"), StrEq("3")));
}

TEST(NamedArg, DynamicFor) // NOLINT
{
  constexpr auto v1_tuple = named_tuple("1"_na(1), "2"_na(2), "3"_na(3));
  std::vector<int> v_ints;
  std::vector<std::string> v_strings;
  auto values_missing = dynamic_for_each(
      [&v_ints, &v_strings](std::string_view name, int value) {
        v_ints.push_back(value);
        v_strings.emplace_back(name);
      },
      v1_tuple, {"1", "2"});
  EXPECT_THAT(values_missing, Eq(0));
  EXPECT_THAT(v_ints, ElementsAre(1, 2));
  EXPECT_THAT(v_strings, ElementsAre(StrEq("1"), StrEq("2")));

  v_ints.clear();
  v_strings.clear();
  values_missing = dynamic_for_each(
      [&v_ints, &v_strings](std::string_view name, int value) {
        v_ints.push_back(value);
        v_strings.emplace_back(name);
      },
      v1_tuple, {"1", "4"});
  EXPECT_THAT(values_missing, Eq(1));
  EXPECT_THAT(v_ints, ElementsAre(1));
  EXPECT_THAT(v_strings, ElementsAre(StrEq("1")));
}

TEST(NamedArg, ForEachFunctionOnlyWorksWithChosenElements) // NOLINT
{
  auto v1_tuple =
      named_tuple("1"_na(1), "2"_na(2), "3"_na(std::string_view("not an int")));

  std::vector<int> values;
  tuple_for_each(
      [&values](auto const &, int &value) { values.push_back(value); },
      v1_tuple, template_string_list_t<"1">{});
  EXPECT_THAT(values, ElementsAre(1));
  values.clear();
  tuple_for_each(
      [&values](auto const &, int &value) { values.push_back(value); },
      v1_tuple, template_string_list_t<"1", "2">{});
  EXPECT_THAT(values, ElementsAre(1, 2));
}

} // namespace dooc
