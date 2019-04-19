
class Class
  @@newMethod = true

  def invariant(&bloqueCondicion)
    methodAdded= self.singleton_class.instance_method(:method_added)

    self.singleton_class.define_method(:method_added) do |*args,&bloqueMethodAdded|
      if(@@newMethod) then
        @@newMethod=false
      methodAdded.bind(self.singleton_class).(*args,&bloqueMethodAdded)

      nombreDelMetodo=args.first
      puts nombreDelMetodo

      m = instance_method(nombreDelMetodo)

        define_method(nombreDelMetodo) do |*args2, &bloqueMetodo|
          proc{unless self.instance_exec &bloqueCondicion then raise "La condicion de algun invariant no se cumple" end}.call
          m.bind(self).(*args2, &bloqueMetodo)
          proc{unless self.instance_exec &bloqueCondicion then raise "La condicion de algun invariant no se cumple" end}.call
        end
      end
      @@newMethod=true
    end
  end
end

class MiClase

  attr_accessor :energia

  invariant{energia<30}

  def initialize()
    self.energia=25
  end

  def mensaje_1
    self.energia+=1
  end

end



