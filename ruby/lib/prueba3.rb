class Class
  def invariant(&bloqueCondicion)
    methodAdded= self.singleton_class.instance_method(:method_added)
    self.singleton_class.define_method(:method_added) do |*args,&bloqueMethodAdded|

      result=methodAdded.bind(self.singleton_class).(*args,&bloqueMethodAdded)
      nombreDelMetodo=args.first
      puts nombreDelMetodo
      m = instance_method(nombreDelMetodo)
        define_method(nombreDelMetodo) do |*args2, &bloqueMetodo|
          proc{unless self.instance_exec &bloqueCondicion then raise "La condicion de algun invariant no se cumple" end}.call
          m.bind(self).(*args2, &bloqueMetodo)
          proc{unless self.instance_exec &bloqueCondicion then raise "La condicion de algun invariant no se cumple" end}.call
        end
      result
    end
  end
end

class MiClase

  attr_accessor :energia

  def initialize()
    self.energia=25
    puts "tu vieja"
  end

  invariant{energia<30}

  def mensaje_1
    self.energia+=1
  end

end



