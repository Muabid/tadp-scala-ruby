require 'rspec'
require_relative '../lib/pruebaFinal'

describe 'invariant' do

  klass = Class.new do
    extend Contract

    attr_accessor :energia

    def initialize
      self.energia=20
    end

    invariant{energia<=20}
    invariant{energia>10}


    def tomarCoca
      self.energia+=1
    end
    def hacerEjercicio(horas)
      self.energia-=horas
    end
  end

  it 'que el invariant de error si se suma la energia al tomar coca' do
    expect{klass.new.tomarCoca}.to raise_error(InvariantError)
  end
  it 'que no se triggeree ningun error al bajar la energia haciendo ejercicio por una hora' do
    expect(klass.new.hacerEjercicio(1)).to eq(19)
  end
  it 'que de un error al hacer ejercicio por 10 horas ya que no se cumple el segundo invariant' do
    expect{klass.new.hacerEjercicio(10)}.to raise_error(InvariantError)
  end
end