module Contract
  @@newMethod = true

  def self.extended(mod)
    mod.class_variable_set :@@pres, [Pre.new(proc {true})]
    mod.class_variable_set :@@posts, [Post.new(proc {true})]
    mod.class_variable_set :@@invariants, [Invariant.new(proc {true})]
    puts mod
  end

  def before_and_after_each_call(before, after,klass = ContractType)
    puts klass
    invariants = self.class_variable_get(:@@invariants)
    puts invariants
    pres = self.class_variable_get(:@@pres)
    puts pres
    posts = self.class_variable_get(:@@posts)
    puts posts
    klass.addToContract(before,after,self)

    presCopia=pres
    postCopia=posts

    self.define_singleton_method(:method_added) do |method|
      pres.last.instance_variable_set :@method,method
      posts.last.instance_variable_set :@method,method
      puts pres.last
      puts posts.last
      if (@@newMethod) then
        @@newMethod = false
        m = instance_method(method.to_s)
        define_method(method) do |*args, &block|
         # presCopia.last.call(self,method)
         # puts presCopia
          klass.new(before).call(self ,method,*args)
          result = m.bind(self).(*args, &block)
          puts result
          invariants.each {|x| x.call(self,method,result)}
          #puts invariants
          #postCopia.last.call(self,method,result)
          klass.new(after).call(self ,method,result)
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

    def self.addToContract(pre,post,klass)
      puts klass
      klass.class_variable_get(:@@invariants).push(Invariant.new(post))
    end
  end

  class Pre < ContractType
    def call(object,method,*args)

        unless super(object,method,*args) then
          raise "ASFJAJFJSAI"
        end

    end

    def self.addToContract(pre,post,klass)
      klass.class_variable_get(:@@pres).push(Post.new(post))
    end
  end

  class Post < ContractType

    def self.addToContract(pre,post,klass)
      klass.class_variable_get(:@@posts).push(Post.new(pre))
    end

    def call(object,method,*args)

        unless super(object,method,*args) then
          raise "Post rompio mostro"
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

  invariant { 2 == 2}


  pre {energia > 10 }
  post {|res| res <10}
  def hola
    @energia = 18
    3463
  end

end

puts Clase.new.hola


