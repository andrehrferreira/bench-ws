defmodule ServerElixirTest do
  use ExUnit.Case
  doctest ServerElixir

  test "greets the world" do
    assert ServerElixir.hello() == :world
  end
end
