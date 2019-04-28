require 'rspec'

require_relative '../lib/pruebaFinal'

describe 'post' do

  klass = Class.new do
    extend Contract

    attr_accessor :energia

    def initialize
      self.energia=20
    end

    post{|res| res==5}
    def serUn5digo
      5
    end

    post{|res| res==6}
    def tuvieja
      5
    end

    post{energia>50}
    def subirEnergiaTuMami
      self.energia+=1
    end

    post{energia==21}
    def laEnergiaDaBienEnLaPrecondicionlpm
      self.energia+=1
    end

    post{|res| res==5*cartoncitos}
    def postSeCumpleConResultadoLTA(cartoncitos)
      5*cartoncitos
    end

  end

  it 'la intancia llama a serUn5digo y no rompe por cumplir que el resultado es 5 en el post' do
    expect(klass.new.serUn5digo).to eq 5
  end
  it 'la isntancia llama a tuvieja y rompe por no cumplir que el resultado es 5' do
    expect{klass.new.tuvieja}.to raise_error PostError
  end
  it 'la instancia llama a subirEnergiaTuMami y rompe por no cumplir que la energia sea mayor a 50' do
    expect{klass.new.subirEnergiaTuMami}.to raise_error PostError
  end
  it 'la instancia llama laEnergiaDaBienEnLaPrecondicionlpm y no rompe por cumplir con tener esa energia' do
    expect(klass.new.laEnergiaDaBienEnLaPrecondicionlpm).to eq 21
  end
  it 'la instancia llama a postSeCumpleConResultadoLTA y siemrpe da bien por cumplir la postCondicion' do
    expect(klass.new.postSeCumpleConResultadoLTA(5)).to eq 25
  end

end