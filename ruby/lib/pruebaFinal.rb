module Contract
  @@newMethod = true

  def self.extended(mod)
    mod.class_variable_set :@@pres, [Pre.new(proc {true})]
    mod.class_variable_set :@@posts, [Post.new(proc {true})]
    mod.class_variable_set :@@invariants, [Invariant.new(proc {true})]
  end

  def before_and_after_each_call(before, after,klass = ContractType)

    klass.addToContract(before,after,self)

    self.define_singleton_method(:method_added) do |method|
      if (@@newMethod) then
        @@newMethod = false

        invariants = self.class_variable_get(:@@invariants)
        pres = self.class_variable_get(:@@pres)
        posts = self.class_variable_get(:@@posts)

        presCopia=pres.map(&:clone)
        postCopia=posts.map(&:clone)

        pres.push(Pre.new(proc {true}))

        posts.push(Post.new(proc {true}))

        m = instance_method(method.to_s)

        listaDeNombresParametros= m.parameters.flat_map {|tupla| tupla.last}


        define_method(method) do |*args, &block|

          presCopia.last.call(self,method,nil,*args,listaDeNombresParametros)

          result = m.bind(self).(*args, &block)
          invariants.each {|x| x.call(self,method,result,nil,nil)}

          postCopia.last.call(self,method,result,*args,listaDeNombresParametros)
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
    before_and_after_each_call(block,proc{true},Pre)
  end

  def post(&block)
    before_and_after_each_call(proc{true},block,Post)
  end

  class ContractType
    attr_accessor :proc,:method

    def initialize(proc)
      @proc = proc
    end

    def call(object,method,result,*args,listaDeNombres)
      copy = object.clone
      if(listaDeNombres!=nil ) then
      values = listaDeNombres.zip(args)
      values.each do |tupla|
        copy.define_singleton_method (tupla[0]) do
          tupla[1]
        end
      end
      end
      copy.instance_exec(result,&@proc)
    end

  end

  class Invariant < ContractType
    def call(object,method,result,*args,listaDeNombres)
      result = super(object,method,result,*args,listaDeNombres)
      unless result then
        raise "No se cumple el invariant"
      end
    end

    def self.addToContract(pre,post,klass)
      klass.class_variable_get(:@@invariants).push(Invariant.new(post))
    end
  end

  class Pre < ContractType
    def call(object,method,result,*args,listaDeNombres)

        unless super(object,method,result,*args,listaDeNombres) then
          raise "Pre no se cumple"
        end

    end

    def self.addToContract(pre,post,klass)
      klass.class_variable_get(:@@pres).push(Pre.new(pre))
    end
  end

  class Post < ContractType

    def self.addToContract(pre,post,klass)
      klass.class_variable_get(:@@posts).push(Post.new(post))
    end

    def call(object,method,result,*args,listaDeNombres)

        unless super(object,method,result,*args,listaDeNombres) then
          raise "Post rompio mostro"
        end

    end

  end
end

class Object

end

class Clase
  extend Contract
  attr_accessor :energia

  def initialize
    @energia = 10
  end

  invariant { energia < 11}
  invariant { true }

  pre {energia > 9 }
  post {|res| res <10}
  def hola
    @energia = 10
  end

  pre {energia > 11}
  post {false}
  def chau(energia)
    2
  end

  pre {limon == 1}
  post {|res| res == limon}
  def chupar(limon)
    limon
  end

end



