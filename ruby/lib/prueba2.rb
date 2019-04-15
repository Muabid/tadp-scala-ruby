
class Class

  def validate(&bloque)
    condition= (proc &bloque).call
    unless condition then raise "La condicion de algun invariant no se cumple" end
  end

  def invariant(&bloque)
    self.instance_methods(false).select { |x| sl}.each do |name|
      m = instance_method(name)
      define_method(name) do |*args, &block|
        validate &bloque
        m.bind(self).(*args, &block)
        validate &bloque
      end
    end
  end
end
class MiClase

  attr_accessor :energia

  def initialize()
    self.energia=10
  end

  #invariant{energia<30}

  def mensaje_1
    self.energia+=1
  end

end