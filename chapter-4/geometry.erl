-module(geometry).
-export([area/1, perimeter/1]).

area({rectangle, Width, Height}) -> Width * Height;
area({square, Side})		 -> Side * Side;
area({circle, Radius})		 -> math:pi() * (Radius * Radius);
area({r_triangle, Base, Height}) -> 0.5 * Base * Height.

perimeter({rectangle, Width, Height}) -> (Width * 2) + (Height * 2);
perimeter({square, Side}) 	      -> Side * 4;
perimeter({circle, Radius})	      -> math:pi() * (Radius * 2);
perimeter({r_triangle, Base, Height}) -> 
	Base + Height + math:sqrt((Base * Base) + (Height * Height)). 
