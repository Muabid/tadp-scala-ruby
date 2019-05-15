class InvariantError < RuntimeError
end

class PreError < RuntimeError
end

class PostError < RuntimeError
end

module Contract
  @@newMethod = true

  def self.extended(mod)
    mod.class_variable_set :@@pres, ContractType.buildTrue
    mod.class_variable_set :@@posts, ContractType.buildTrue
    mod.class_variable_set :@@invariants, [ContractType.buildTrue]
  end

  def before_and_after_each_call

    self.define_singleton_method(:method_added) do |method|
      if (@@newMethod) then
        @@newMethod = false

        invariants = self.class_variable_get(:@@invariants)
        pre = self.class_variable_get(:@@pres)
        post = self.class_variable_get(:@@posts)

        self.class_variable_set(:@@pres,ContractType.buildTrue)
        self.class_variable_set(:@@post,ContractType.buildTrue)

        m = instance_method(method.to_s)

        params= m.parameters.flat_map {|param,value| value}

        define_method(method) do |*args, &block|

          pre.call(self,method,nil,*args,params)

          result = m.bind(self).(*args, &block)
          invariants.each {|x| x.call(self,method,result,nil,nil)}

          post.call(self,method,result,*args,params)
          result
        end
        @@newMethod = true
      end
    end

  end

  def addInvariant(invariant)
    self.class_variable_get(:@@invariants).push(invariant)
  end

  def invariant(&block)
    addInvariant(Invariant.new(block))
    before_and_after_each_call
  end

  def pre(&block)
    self.class_variable_set(:@@pres,Pre.new(block))
    before_and_after_each_call
  end

  def post(&block)
    self.class_variable_set(:@@posts,Post.new(block))
    before_and_after_each_call
  end

  class ContractType
    attr_accessor :proc,:method

    def initialize(proc)
      @proc = proc
    end

    def self.buildTrue
      ContractType.new(proc {true})
    end

    def call(object,method,result,*args,params)
      copy = object.clone
      if(params!=nil ) then
      values = params.zip(args)
      values.each do |param, value|
        copy.define_singleton_method (param) do
          value
        end
      end
      end
      copy.instance_exec(result,&@proc)
    end

  end

  class Invariant < ContractType

    def call(object,method,result,*args,params)
      result = super(object,method,result,*args,params)
      unless result then
         raise InvariantError
      end
    end

  end

  class Pre < ContractType

    def call(object,method,result,*args,params)
        unless super(object,method,result,*args,params) then
          raise PreError
        end

    end

  end

  class Post < ContractType

    def call(object,method,result,*args,params)
        unless super(object,method,result,*args,params) then
          raise PostError
        end

    end

  end
end



