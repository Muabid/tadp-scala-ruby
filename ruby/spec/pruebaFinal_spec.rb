require 'rspec'

require_relative '../lib/pruebaFinal'


describe 'pruebaFinal' do

    klass = Class.new do
      extend Contract

      attr_accessor :energia,:ganas_de_vivir

      def initialize
        self.energia=20
        self.ganas_de_vivir = 10
      end

      invariant{energia<=20}

      def reducirEnergia
        self.energia-=1
      end

      pre {param > 0}
      def otroMetodo(param)
        param
      end

      post {|res| res >1}
      def tomarCoca(param)
        param
      end

      def tomarMonster(litros)
          self.energia+=litros
      end

      pre {energia < 20}
      def metodo_para_probar_prioridad(energia)

      end

      pre {energia > 10}
      post {|nuevas_ganas_de_vivir| ganas_de_vivir > nuevas_ganas_de_vivir}
      def caminar(cuadras)
        self.ganas_de_vivir - 4
      end


    end

    it 'Lanza PostError porque retorna 0 y debe ser mayor a 1 el resultado, que es igual al parametro que recibe' do
      expect{klass.new.tomarCoca(0)}.to raise_error(PostError)
    end
    it 'si se manda una instancia a tomar coca con un parametro mayor a uno todo sale bien' do
      expect(klass.new.tomarCoca(10)).to eq 10
    end
    it 'si se manda una instancia a tomar monster con una cantidad de litros mayor a 0, el invariant deberia romper' do
      expect{klass.new.tomarMonster(100)}.to raise_error(InvariantError)
    end
    it 'si se manda una instancia a tomar monster con una cantidad de litros de 0, todo sale bien porque sigue cumpliendose el invariant' do
      expect(klass.new.tomarMonster(0)).to eq(20)
    end
    it 'se priorizan los parametros a la hora de validar contratos en el caso de que tanto parametros como atributos tengan el mismo nombre' do
      expect{pedro=klass.new
      pedro.reducirEnergia
      pedro.metodo_para_probar_prioridad(50)}.to raise_error(PreError)
    end
    it 'se cumplen tanto pre como post' do
      expect(klass.new.caminar(5)).to eq 6
    end
end
