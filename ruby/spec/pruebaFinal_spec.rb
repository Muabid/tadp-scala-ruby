require 'rspec'

require_relative '../lib/pruebaFinal'

class MiClass
  attr_accessor :energia,:gordura

  def initialize()
    self.energia=25
    self.gordura=10
  end
  invariant{energia>5}
  invariant{energia<30}

  pre{energia>10}
  post{energia==1}

  def hacerEjercicio
    self.energia-=1
  end

  post{gordura==10}

  def comerNachos
    self.gordura+=1
  end

  #post{|result| result==energia+litros}
  def tomarMonster(litros)
    self.energia+=litros
  end

end




describe 'pruebaFinal' do
  before do
    # Do nothing
  end

  after do
    # Do nothing
  end

  context 'deberia tirar error' do
  it 'si se manda una instancia a comer nachos por no cumplir post{gordura==10}' do
    expect{MiClass.new.comerNachos}.to raise_error(RuntimeError)
  end
  it 'si se manda una instancia a hacer ejercicio por no cumplir post{energia==1}' do
      expect{MiClass.new.hacerEjercicio}.to raise_error(RuntimeError)
    end

     #it 'si se manda una instancia a tomar monster ya que su algoritmo no es multiplicando sino sumando' do
      #expect{MiClass.new.tomarMonster(5)}.to raise_error(RuntimeError)
     #end
  end
end