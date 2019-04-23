require 'test/unit'
require '../../lib/pruebaFinal'

class Persona

  attr_accessor :energia,:gordura

  invariant{energia>30 && energia <50}
  invariant{gordura>5}

  def initialize()
    self.energia=33
    self.gordura=20
  end

  pre{energia + gordura < 55}
  def subirEnegia
    self.energia+=10
  end

  post {energia - gordura >0}

  def desengordar
    self.gordura-=1
  end

end


class MyTest < Test::Unit::TestCase

  # Called before every test method runs. Can be used
  # to set up fixture information.
  def setup

  end


  def test_cumple_pre
    asert_raise("RuntimeError") do
      Persona.new.subirEnegia
    end
  end

  # Called after every test method runs. Can be used to tear
  # down fixture information.

  def teardown
    # Do nothing
  end

end