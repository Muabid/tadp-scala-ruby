
class Class
  @@newMethod = true

  #Esta solucion no permite dos invariant

  def invariant(&bloqueCondicion)
    puts "mensaje 1"
    methodAdded= self.singleton_class.instance_method(:method_added)
    puts self
    puts "mensaje 2"
    self.singleton_class.define_method(:method_added) do |*args,&bloqueMethodAdded|
      if(@@newMethod) then
        @@newMethod=false
        puts "mensaje 3"
      puts self
      methodAdded.bind(self).(*args,&bloqueMethodAdded)
      nombreDelMetodo=args.first

      m = instance_method(nombreDelMetodo)

        define_method(nombreDelMetodo) do |*args2, &bloqueMetodo|
          proc{unless self.instance_exec &bloqueCondicion then raise "La condicion de algun invariant no se cumple" end}.call if nombreDelMetodo.to_s!= "initialize"
          result =m.bind(self).(*args2, &bloqueMetodo)
          proc{unless self.instance_exec &bloqueCondicion then raise "La condicion de algun invariant no se cumple" end}.call
          result
        end
      @@newMethod=true
      end
    end
  end
end

class MiClase

  attr_accessor :energia,:gordura

  invariant{energia<30}
  invariant{gordura>5}

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



