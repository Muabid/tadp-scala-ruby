module Interfaz

  def self.included(base)
    base.send(:extend, Before_after)
  end

  module Before_after
    def before_after(before,after)
      @hooks.each do |name|
        m = instance_method(name)
        define_method(name) do |*args, &block|
          before.call
          m.bind(self).(*args, &block)
          after.call
        end
      end
    end

    def before_after_each_call(*method_name,before,after)
      @hooks = method_name
      before_after(before,after)
    end

    def hooks
      @hooks ||= []
    end

  end
end

class Class
  include Interfaz

  def excepcionBlock(&bloque)
    proc {
         unless ((proc &bloque).call) then raise "No se cumple alguna condicion de clase" end
    }
  end

  def invariant (&bloque)
    self.instance_methods(false).each {|metodo| before_after_each_call(method_added(metodo),proc {},excepcionBlock(&bloque))}
  end


end


class MiClase

  attr_accessor :energia

  def initialize()
    self.energia=10
  end

  invariant{energia>10}

  def mensaje_1
    self.energia+=1
  end

end






