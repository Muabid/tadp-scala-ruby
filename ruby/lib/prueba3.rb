class ProcAndContext
  attr_accessor :proc,:contexto
  def initialize(proc)
    self.proc=proc
  end

  def executeInContext
    contexto.instance_eval &proc
  end
end

class Class
  @@newMethod = true
  @@invariantBlocks=[ProcAndContext.new(proc{true})]
  @@validation= proc{|procAndContext|
  unless procAndContext.executeInContext then raise "La condicion de algun invariant no se cumple" end}

  def invariant(&bloqueCondicion)

    @@invariantBlocks.push(ProcAndContext.new(bloqueCondicion))

    self.singleton_class.define_method(:method_added) do |*args,&bloqueMethodAdded|
      if(@@newMethod) then
        @@newMethod=false
        puts self
      nombreDelMetodo=args.first
      m = instance_method(nombreDelMetodo)

        define_method(nombreDelMetodo) do |*args2, &bloqueMetodo|

          @@invariantBlocks.each{|procAndContext|
            procAndContext.contexto=self
          @@validation.call(procAndContext) if nombreDelMetodo.to_s!= "initialize"} #invariant

          result =m.bind(self).(*args2, &bloqueMetodo)

          @@invariantBlocks.each{|procAndContext|
            procAndContext.contexto=self
            @@validation.call(procAndContext)} #invariant

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



