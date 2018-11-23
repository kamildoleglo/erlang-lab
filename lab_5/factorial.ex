defmodule Factorial do 
	def factorial(upto) do 
		case upto do
			x when x < 2 -> 
				1
			x ->
				x * factorial(x-1)
		end
	end
end
