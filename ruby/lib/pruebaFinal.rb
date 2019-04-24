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
    unless procAndContext.executeInContext then raise "Some contract is being broken" end}
  @@preBlock=ProcAndContext.new(proc{true})
  @@postBlock=ProcAndContext.new(proc{true})

  def defineMethodAdded
    self.singleton_class.define_method(:method_added) do |*args,&bloqueMethodAdded|
      if(@@newMethod) then
        @@newMethod=false
        nombreDelMetodo=args.first
        m = instance_method(nombreDelMetodo)
        pasoPrevio=@@preBlock
        pasoPosterior=@@postBlock
        @@preBlock=ProcAndContext.new(proc{true})
        @@postBlock=ProcAndContext.new(proc{true})
        define_method(nombreDelMetodo) do |*args2, &bloqueMetodo|

          pasoPrevio.contexto=self
          @@validation.call(pasoPrevio) if nombreDelMetodo.to_s!= "initialize" #precondicion

          @@invariantBlocks.each{|procAndContext|
            procAndContext.contexto=self
            @@validation.call(procAndContext) if nombreDelMetodo.to_s!= "initialize"} #invariant

          result =m.bind(self).(*args2, &bloqueMetodo) #definicion del metodo

          @@invariantBlocks.each{|procAndContext|
            procAndContext.contexto=self
            @@validation.call(procAndContext)} #invariant

          pasoPosterior.contexto=self
          @@validation.call(pasoPosterior) #postcondicion

          result
        end
        @@newMethod=true
      end
    end
  end

  def pre(&bloqueCondicion)
      @@preBlock=ProcAndContext.new(bloqueCondicion)
      defineMethodAdded
  end

  def post(&bloqueCondicion)
    @@postBlock=ProcAndContext.new(bloqueCondicion)
    defineMethodAdded
  end


  def invariant(&bloqueCondicion)
    @@invariantBlocks.push(ProcAndContext.new(bloqueCondicion))
    defineMethodAdded
  end
end

class MiClass
  attr_accessor :energia,:gordura

  def initialize()
    self.energia=25
    self.gordura=10
  end
  #invariant{energia>5}
  #invariant{energia<30}

  pre{energia>10}
  post{energia==1}

  def hacerEjercicio
    self.energia-=1
  end

  post{gordura==10}

  def comerNachos
    self.gordura+=1
  end

  post{|result| result==energia+litros}
  def tomarMonster(litros)
    self.energia+=litros
  end

end




