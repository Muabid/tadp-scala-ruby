require 'rspec'
require_relative '../lib/pruebaFinal'

describe 'pre' do
  klass = Class.new do
    extend Contract

    attr_accessor :energia

    def initialize
      @energia=20
    end

    pre{litros.is_a?(Integer)}
    def tomarCoca(litros)
      @energia += 5
    end

    pre{energia > 1}
    def hacerEjercicio(horas)
      @energia -= horas
    end

    pre{false}
    def metodoInllamable
      nil
    end

    def irARezar
      "fui a rezar"
    end

  end

  it 'una instancia cumple con todas las pre condiciones sin errores' do
    expect(klass.new.tomarCoca(5)).to eq 25
  end

  it 'si se le pasa a tomar coca cola un entero todo sale bien' do
    expect(klass.new.tomarCoca(1)).to eq(25)
  end

  it 'no se cumple pre condicion de tomarCoca (no se le pasa un entero)' do
    expect{klass.new.tomarCoca("lala")}.to raise_error(PreError)
  end

  it 'metodo irARezar no es afectado por el pre de metodo inllamable' do
    expect(klass.new.irARezar).to eq "fui a rezar"
  end

  it 'metodo inllamable no se puede llamar porque siempre rompe por el pre' do
    expect{klass.new.metodoInllamable}.to raise_error PreError
  end

end