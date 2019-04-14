
class Class

  def invariant(&bloque)
    before_and_after_each_call(proc{},proc &bloque)
  end
  def before_and_after_each_call(bloqueAntes,bloqueDespues)

  end

end


class MiClass
  attr_accessor :energia,:ataque

  def initialize(energia,ataque)
    self.energia=energia
    self.ataque=ataque
  end

  pre{energia>50}
  post{ataque>50}
  def comer ()
    self.energia+=1
    self.ataque+=1
  end
  pre{energia<50}
  def cagar
    self.energia-=1
    self.ataque-=1
  end

end
