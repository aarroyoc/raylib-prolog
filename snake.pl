:- use_module(raylib).

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(time)).
:- use_module(library(format)).
:- use_module(library(random)).
:- use_module(library(clpz)).

state_get([], _).
state_get([Key-Value|Ps], S) :-
    get_assoc(Key, S, Value),
    state_get(Ps, S).
state_get(Pairs), [S] -->
    [S],
    { state_get(Pairs, S) }.

state_put(Ps), [S] -->
    [S0],
    { state_put_(Ps, S0, S) }.

state_put_([], S, S).
state_put_([Key-Value|Ps], S0, S) :-
    put_assoc(Key, S0, Value, S1),
    state_put_(Ps, S1, S).

start :-
    init_window(400, 400, "Snake"),
    set_target_fps(60),
    get_time(Time),
    list_to_assoc(
	[
	    snake-[10-10, 10-11],
	    food-[],
	    time-Time,
	    direction-up,
	    look_at-up,
	    ticks-0,
	    end-false
	], State),
    loop(State).

loop(State0) :-
    (
	window_should_close ->
	close_window,
	halt
    ;   (
	inputs(Inputs),!,
	phrase(update(Inputs), [State0], [State]),!,
	render(State),!,
	loop(State)
        )
    ).

inputs(Inputs) :-
    phrase(inputs_, Inputs).

inputs_ -->
    inputs_right_,
    inputs_left_,
    inputs_up_,
    inputs_down_.

inputs_right_ -->
    (
	{ is_key_down(right) },
	[right]
    ) | [].

inputs_left_ -->
    (
	{ is_key_down(left) },
	[left]
    ) | [].

inputs_up_ -->
    (
	{ is_key_down(up) },
	[up]
    ) | [].

inputs_down_ -->
    (
	{ is_key_down(down) },
	[down]
    ) | [].

update(Inputs) -->
    state_get([time-Time, end-false]),
    { \+ update_needed(Time, _) },
    snake_dir(Inputs).

update(Inputs) -->
    state_get([time-T0, ticks-Ticks0, end-false]),
    state_put([time-T, ticks-Ticks, end-true]),
    { update_needed(T0, T) },
    { Ticks is Ticks0 + 1 },
    snake_dir(Inputs),
    move_snake,
    crash_snake(yes).

update(Inputs) -->
    state_get([time-T0, ticks-Ticks0, end-false]),
    state_put([time-T, ticks-Ticks]),
    { update_needed(T0, T) },
    { Ticks is Ticks0 + 1 },
    snake_dir(Inputs),
    move_snake,
    crash_snake(no),
    food_update,
    eat_update.

update(_Inputs) -->
    state_get([end-true]).

update_needed(Time0, Time1) :-
    get_time(Time1),
    Time1 > Time0 + 0.3 .

snake_dir(Inputs) -->
    state_get([direction-Dir0, look_at-LookAt]),
    state_put([direction-Dir]),
    { snake_dir(Inputs, Dir0, LookAt, Dir) }.

snake_dir(Inputs, _, LookAt, left) :-
    member(left, Inputs),
    \+ member(right, Inputs),
    LookAt \= right,!.

snake_dir(Inputs, _, LookAt, right) :-
    member(right, Inputs),
    \+ member(left, Inputs),
    LookAt \= left,!.

snake_dir(Inputs, _, LookAt, up) :-
    member(up, Inputs),
    \+ member(down, Inputs),
    LookAt \= down,!.

snake_dir(Inputs, _, LookAt, down) :-
    member(down, Inputs),
    \+ member(up, Inputs),
    LookAt \= up,!.

snake_dir(_Inputs, Current, _LookAt, Current).

crash_snake(yes) -->
    state_get([snake-[Head|Tail]]),
    { member(Head, Tail) }.
crash_snake(no) -->
    state_get([snake-[Head|Tail]]),
    { \+ member(Head, Tail) }.

move_snake -->
    state_get([snake-Snake0, direction-Dir]),
    state_put([snake-Snake, look_at-Dir]),
    { move_snake(Dir, Snake0, Snake) }.

move_snake(left, Snake0, Snake) :-
    append([SnakeHead|SnakeTail], [_], Snake0),
    SnakeHead = X-Y,
    NewX is (X - 1) mod 20,
    Snake = [NewX-Y,SnakeHead|SnakeTail].

move_snake(right, Snake0, Snake) :-
    append([SnakeHead|SnakeTail], [_], Snake0),
    SnakeHead = X-Y,
    NewX is (X + 1) mod 20,
    Snake = [NewX-Y,SnakeHead|SnakeTail].

move_snake(up, Snake0, Snake) :-
    append([SnakeHead|SnakeTail], [_], Snake0),
    SnakeHead = X-Y,
    NewY is (Y - 1) mod 20,
    Snake = [X-NewY,SnakeHead|SnakeTail].

move_snake(down, Snake0, Snake) :-
    append([SnakeHead|SnakeTail], [_], Snake0),
    SnakeHead = X-Y,
    NewY is (Y + 1) mod 20,
    Snake = [X-NewY,SnakeHead|SnakeTail].

food_update -->
    state_get([ticks-Ticks, snake-Snake, food-Food0]),
    state_put([food-Food]),
    { food_update(Ticks, Snake, Food0, Food) }.

food_update(Ticks, Snake, Food0, Food) :-
    F is Ticks mod 15,
    (
	F = 1 ->
	(
	    food_position(Snake, X-Y),
	    Food = [X-Y|Food0]
	)
    ;   Food0 = Food
    ).

food_position(Snake, X-Y) :-
    setof(SnakePos, SnakeX^SnakeY^(
	      [SnakeX, SnakeY] ins 0..20, 
	      SnakePos = SnakeX-SnakeY,
	      label([SnakeX, SnakeY]),
	      \+ member(SnakePos, Snake)
	  ), FreeSpots),
    random_choice(FreeSpots, X-Y).

random_choice(Xs, X) :-
    length(Xs, N),
    random_integer(0, N, R),
    nth0(R, Xs, X).

eat_update -->
    state_get([snake-Snake0, food-Food0]),
    state_put([snake-Snake, food-Food]),
    { eat_update(Snake0, Snake, Food0, Food) }.

eat_update(Snake0, Snake, Food0, Food) :-
    Snake0 = [SnakeHead|SnakeTail],
    Snake = [SnakeHead, SnakeHead|SnakeTail],
    select(SnakeHead, Food0, Food).

eat_update(Snake0, Snake, Food0, Food) :-
    Snake0 = [SnakeHead|_SnakeTail],
    Snake0 = Snake,
    Food0 = Food,
    \+ member(SnakeHead, Food0).

render(State) :-
    state_get([snake-Snake, food-Food, end-End], State),
    begin_drawing,
    clear_background(color(255, 255, 255, 255)),
    draw_snake(Snake),
    draw_food(Food),
    draw_score(Snake, End),
    end_drawing.

draw_snake(Snake) :-
    maplist(draw_snake_block, Snake).

draw_snake_block(X-Y) :-
    PixelX is X*20,
    PixelY is Y*20,
    draw_rectangle(PixelX, PixelY, 20, 20, red),
    draw_rectangle_lines(PixelX, PixelY, 20, 20, black).

draw_food(Food) :-
    maplist(draw_food_block, Food).

draw_food_block(X-Y) :-
    PixelX is X*20+10,
    PixelY is Y*20+10,
    draw_circle(PixelX, PixelY, 10, green),
    draw_circle_lines(PixelX, PixelY, 10, black).

draw_score(Snake, End) :-
    length(Snake, Score0),
    Score is Score0 - 2,
    phrase(format_("Score: ~d", [Score]), ScoreText),
    draw_text(ScoreText, 10, 10, 16, gray),
    (
	End = true ->
	draw_text("Game over!", 150, 180, 16, black)
    ; true
    ).

:- initialization(start).
