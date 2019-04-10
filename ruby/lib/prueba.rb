class Prueba

  def materia
    :tadp
  end
end

class Object
  def self.variant(&bool)
      @ejecutarBool=proc &bool
      @ejecutarExepcion= proc {
          unless @ejecutarBool.call then
            raise "No se pudo cumplir una condicion"
          end
      }
      before_and_after_each_call(proc {},@ejecutarExepcion)
  end
end
