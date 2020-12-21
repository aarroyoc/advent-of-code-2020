:- set_prolog_flag(double_quotes, chars).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(pairs)).

input_ingredients([Ingredient|Ingredients]) -->
    string_without(" (", Ingredient),
    " ",
    input_ingredients(Ingredients).

input_ingredients([]) --> [].

input_allergens([Allergen|Allergens]) -->
    string_without(" )", Allergen),
    ((" ", input_allergens(Allergens))|(")", { Allergens = [] })).

input([recipe(Ingredients, Allergens)|Recipes]) -->
    input_ingredients(Ingredients),
    "(contains ",
    input_allergens(Allergens),
    "\n",
    input(Recipes).

input([]) --> eos.

load_data(Recipes) :-
    read_file_to_string('day21/input.dat', String, []),
    string_chars(String, Chars),
    phrase(input(Recipes), Chars).

star(backtracking1, N) :-
    load_data(Recipes),
    recipes_ingredients(Recipes, Ingredients),
    maplist(ingredient_build, Ingredients, IngredientsAllergens),!,
    maplist(recipe_allergens(IngredientsAllergens), Recipes),!,
    convlist(non_allergen_ingredients, IngredientsAllergens, SafeIngredients),
    merge_ingredients(Recipes, AllIngredients),
    foldl(count_safe(SafeIngredients), AllIngredients, 0, N).

star(2, N) :-
    load_data(Recipes),
    allergen_list(Recipes, AllergensList),
    sort(AllergensList, SortedAllergensList),
    group_pairs_by_key(SortedAllergensList, AllergensLists),
    maplist(intersect_ingredients, AllergensLists, AllergensSet),
    allergens(AllergensSet, IngredientsAllergens),
    maplist(pair_term, IngredientsAllergens, TermIngredientsAllergens),
    sort(2, @=<, TermIngredientsAllergens, SortedIngredientsAllergens),
    forall(member(danger(I, _), SortedIngredientsAllergens),format('~s,', [I])).

star(1, N) :-
    load_data(Recipes),
    allergen_list(Recipes, AllergensList),
    sort(AllergensList, SortedAllergensList),
    group_pairs_by_key(SortedAllergensList, AllergensLists),
    maplist(intersect_ingredients, AllergensLists, AllergensSet),
    allergens(AllergensSet, IngredientsAllergens0),
    recipes_ingredients(Recipes, Ingredients),
    maplist(ingredient_build, Ingredients, IngredientsAllergens1),
    union(IngredientsAllergens0, IngredientsAllergens1, IngredientsAllergens),
    convlist(non_allergen_ingredients, IngredientsAllergens, SafeIngredients),
    merge_ingredients(Recipes, AllIngredients),
    foldl(count_safe(SafeIngredients), AllIngredients, 0, N).

pair_term(Ingredient-Allergen, danger(Ingredient, Allergen)).

allergens(Set, []) :-
    forall(member(_-List, Set), length(List, 0)).
allergens(Set, Allergens) :-
    member(Allergen-List, Set),
    length(List, 1),
    [Ingredient] = List,
    maplist(remove_ingredient(Ingredient), Set, Set0),
    allergens(Set0, Allergens0),
    append([Ingredient-Allergen], Allergens0, Allergens).

remove_ingredient(Ingredient, Allergen-ListIn, Allergen-ListOut) :-
    delete(ListIn, Ingredient, ListOut).

intersect_ingredients(Allergen-List, Allergen-Set) :-
    intersect_ingredients_(List, Set).

intersect_ingredients_([X], X).
intersect_ingredients_([X|Xs], S) :-
    intersect_ingredients_(Xs, S0),
    intersection(S0, X, S).

allergen_list([], []).
allergen_list([recipe(Ingredients, Allergens)|Recipes], List) :-
    allergen_list(Recipes, List0),
    findall(Elem,(
        member(Allergen, Allergens),
        Elem = Allergen-Ingredients
    ), List1),
    append(List0, List1, List).


count_safe(SafeIngredients, Ingredient, In, Out) :-
    ( member(Ingredient, SafeIngredients) ->
        Out is In + 1
    ;   Out is In
    ).

merge_ingredients([], []).
merge_ingredients([recipe(I, _)|Recipes], Ingredients) :-
    merge_ingredients(Recipes, Ingredients0),
    append(Ingredients0, I, Ingredients).

non_allergen_ingredients(Ingredient-Allergen, Ingredient) :- var(Allergen).

no_duplicated_allergens(IngredientsAllergens) :-
    maplist(get_allergens, IngredientsAllergens, Allergens),
    list_to_set(Allergens, Allergens).

get_allergens(_-Allergen, Allergen).

ingredient_build(Ingredient, Ingredient-_).

recipes_ingredients([], []).
recipes_ingredients([Recipe|Recipes], Ingredients) :-
    recipes_ingredients(Recipes, IngredientsIn),
    Recipe = recipe(X, _),
    union(X, IngredientsIn, Ingredients).

recipe_allergens(IngredientsAllergens, recipe(Ingredients, Allergens)) :-
    maplist(check_allergens(Ingredients, IngredientsAllergens), Allergens).

check_allergens(Ingredients, IngredientsAllergens, Allergen) :-
    member(Ingredient, Ingredients),
    member(Ingredient-Allergen, IngredientsAllergens),
    no_duplicated_allergens(IngredientsAllergens).