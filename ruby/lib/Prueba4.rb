
module Mixin
  @@newMethod = true

  #Esta solucion no permite dos invariant

  def self.before_and_after_each_call(bloqueAntes,bloqueDespues)
      define_method(:method_added)do  |method|
        m = instance_method(method)
        define_method(method) do |*args, &block|
          bloqueAntes.call
          m.bind(self).(*args,&block)
          bloqueDespues.call
        end
      end
  end
end

class MiClase
  include Mixin

  attr_accessor :energia,:gordura

  before_and_after_each_call(proc {puts "hola"},proc {})

  def initialize()
    self.energia=25
    self.gordura=9
  end

  def subirEnegia
    self.energia+=1
  end

  def desengordar
    self.gordura-=1
  end

end




