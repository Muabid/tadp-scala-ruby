module Contract
  @@newMethod = true

  def self.extended(mod)
    mod.class_variable_set :@@befores, []
    mod.class_variable_set :@@afters, []
  end

  def before_and_after_each_call(before, after,klass = ContractType)

    befores = self.class_variable_get(:@@befores)
    afters = self.class_variable_get(:@@afters)

    befores.push(klass.new(before))
    afters.push(klass.new(after))

    self.define_singleton_method(:method_added) do |method|
      befores.last.instance_variable_set :@method,method
      afters.last.instance_variable_set :@method,method

      if (@@newMethod) then
        @@newMethod = false
        m = instance_method(method.to_s)
        define_method(method) do |*args, &block|
          befores.each {|x| x.call(self,method)}
          result = m.bind(self).(*args, &block)
          afters.each {|x| x.call(self,method,result)}
          result
        end
        @@newMethod = true
      end
    end
  end

  def invariant(&block)
    before_and_after_each_call(proc {true}, block,Invariant)
  end

  def pre(&block)
    before_and_after_each_call(block,proc{true},PrePost)
  end

  def post(&block)
    before_and_after_each_call(proc{true},block,PrePost)
  end

  class ContractType
    attr_accessor :proc,:method

    def initialize(proc)
      @proc = proc
    end

    def call(object,method,*args)
      object.instance_exec(*args,&@proc)
    end
  end

  class Invariant < ContractType
    def call(object,method,*args)
      result = super(object,method)
      unless result then
        raise "No se cumple el invariant"
      end
    end
  end

  class PrePost < ContractType
    def call(object,method,*args)
      if method.equal? @method then
        unless super(object,method,*args) then
          raise "ASFJAJFJSAI"
        end
      end
    end
  end
end

class Clase
  extend Contract
  attr_accessor :energia

  def initialize
    @energia = 10
  end

  post {|res| res >10}
  def hola
    @energia = 18
  end

end

puts Clase.new.hola


