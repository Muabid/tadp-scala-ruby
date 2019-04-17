

class Class

  def nonGetterOrSetterMethods
    variablesSinArroba=new.instance_variables.map{|var| var.to_s}.map{|var| var.gsub('@','')}
    instance_methods(false).select do |method|
      !( variablesSinArroba + variablesSinArroba.map{|var| var +'='}).map{|var| var.to_sym}.include? method
    end
  end

  def invariant(&bloque)
    nonGetterOrSetterMethods.each do |name|
      m = instance_method(name)
      define_method(name) do |*args, &block|
        proc{unless self.instance_exec &bloque then raise "La condicion de algun invariant no se cumple" end}.call
        result=m.bind(self).(*args, &block)
        proc{unless self.instance_exec &bloque then raise "La condicion de algun invariant no se cumple" end}.call
        result
      end
    end
  end
end



class MiClase

  attr_accessor :energia

  def initialize()
    self.energia=25
  end

  def mensaje_1
    self.energia+=1
  end

  invariant{energia<30}

end
