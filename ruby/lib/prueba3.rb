
class Class
  @@newMethod = true

  #Esta solucion no permite dos invariant

  def invariant(&bloqueCondicion)

    methodAdded= self.singleton_class.instance_method(:method_added)

    self.singleton_class.define_method(:method_added) do |*args,&bloqueMethodAdded|
      if(@@newMethod) then
        @@newMethod=false


      methodAdded.bind(self.singleton_class).(*args,&bloqueMethodAdded)
      nombreDelMetodo=args.first

      m = instance_method(nombreDelMetodo)

        define_method(nombreDelMetodo) do |*args2, &bloqueMetodo|
          proc{unless self.instance_exec &bloqueCondicion then raise "La condicion de algun invariant no se cumple" end}.call if nombreDelMetodo.to_s!= "initialize"
          m.bind(self).(*args2, &bloqueMetodo)
          proc{unless self.instance_exec &bloqueCondicion then raise "La condicion de algun invariant no se cumple" end}.call

        end
      @@newMethod=true
      end
    end
  end
end

class MiClase

  attr_accessor :energia,:gorduraDePija

  invariant{energia<30}
  invariant{gorduraDePija>5}

  def initialize()
    self.energia=25
    self.gorduraDePija=9
  end

  def subirEnegia
    self.energia+=1
  end

  def agrandarPija
    self.gorduraDePija-=1
  end

end



