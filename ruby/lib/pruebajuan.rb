class ProcAndContext
  attr_accessor :proc,:contexto
  def initialize(proc,context)
    self.proc=proc
    self.contexto=context
  end

  def executeInContext
    contexto.instance_eval &proc
  end
end

class MiClase

  attr_accessor :energia,:gordura


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
