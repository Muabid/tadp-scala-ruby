describe pruebaFinal do
  let(:prueba) {
    class MiClass
      puts "hola"
    end
  }

  describe '#materia' do
    it 'debería pasar este test' do
      expect(1>0).to be true
    end
  end
end