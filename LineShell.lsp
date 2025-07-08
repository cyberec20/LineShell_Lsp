;; -----------------------------------------------------------------------------
;; 📐 LineShell.LSP – Automatiza creación de offsets, cierre, hatch y bloques
;; -----------------------------------------------------------------------------
;; 🔧 PROPÓSITO
;; Esta rutina permite al usuario seleccionar líneas o polilíneas abiertas, generar
;; automáticamente un contorno cerrado mediante offsets simétricos a ambos lados,
;; añadir líneas de cierre en los extremos, unirlos como polilínea cerrada y aplicar
;; un patrón de hatch editable. También permite crear un bloque con todas las
;; entidades generadas, excluyendo las líneas originales.
;;
;; 🛠️ FUNCIONALIDADES PRINCIPALES
;; 1. Solicita la distancia de offset desde la línea central.
;; 2. Genera dos offsets equidistantes para cada entidad.
;; 3. Cierra automáticamente los extremos si la entidad no es cerrada.
;; 4. Une las líneas resultantes en una polilínea cerrada lista para hatch.
;; 5. Mueve la línea original a la capa "Defpoints" (tipo DASHED, color gris).
;; 6. Permite elegir el patrón de hatch desde un desplegable (SOLID, ANSI31, etc).
;; 7. Aplica hatch automático sobre cada contorno creado (color y capa actuales).
;; 8. Pregunta si desea crear un bloque con todas las entidades nuevas (sin incluir
;;    las líneas originales).
;;
;; 🎯 USOS TÍPICOS
;; - Delineación rápida de canales, ductos, muros, tramos de infraestructura, etc.
;; - Aumentar precisión visual sin intervención manual.
;; - Automatizar sombreados y organización de capas.
;;
;; 🧠 REQUISITOS
;; - Solo admite selección de entidades tipo LINE o LWPOLYLINE.
;; - Offset sólo se aplica a entidades válidas y sin errores de geometría.
;; - Hatch se aplica únicamente a contornos correctamente cerrados.
;;
;; 👤 AUTOR: Franklin Rodriguez - https://www.linkedin.com/in/franklinrodriguezacosta/
;; 📅 VERSIÓN: Julio 2025
;; -----------------------------------------------------------------------------

(princ "\nComando disponible: LineShell")
(princ "\nUso: Escriba LineShell en la línea de comandos")
(princ)

(defun C:LineShell (/ *error* ss dist enLst newEntList blkName createBlock
                            doc entName obj objType isClosed off1 off2 off1Ent off2Ent
                            p1Start p2Start p1End p2End close1 close2 ssBlock basePt
                            defpointsLayer oldPeditAccept entList joinedEnt contourList
                            hatchPattern applyHatch hatchEnt hatchLayer hatchColor
                            modelSpace hatchObj)
  ;; Carga las extensiones Visual LISP para acceder a métodos ActiveX
  (vl-load-com)
  ;; Obtiene el documento activo de AutoCAD
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  
  ;;; Función de manejo de errores
  ;; Esta función se ejecuta si ocurre un error o si el usuario cancela el comando
  (defun *error* (msg)
    ;; Si el mensaje no indica una cancelación normal, muestra el error
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*"))
      (princ (strcat "\nError: " msg))
    )
    ;; Restaura la variable PEDITACCEPT a su valor original si fue guardada
    (if oldPeditAccept (setvar "PEDITACCEPT" oldPeditAccept))
    ;; Restaura el eco de comandos
    (setvar "CMDECHO" 1)
    ;; Finaliza la marca de deshacer
    (vla-EndUndoMark doc)
    (princ)
  )
  
  ;; Desactiva el eco de comandos para una ejecución más limpia
  (setvar "CMDECHO" 0)
  ;; Inicia una marca de deshacer para agrupar todas las operaciones
  (vla-StartUndoMark doc)
  ;; Guarda el valor actual de PEDITACCEPT y lo configura para evitar confirmaciones
  (setq oldPeditAccept (getvar "PEDITACCEPT"))
  (setvar "PEDITACCEPT" 1)  ; Desactiva la confirmación de conversión a polilínea
  ;; Inicializa listas para almacenar contornos cerrados y nuevas entidades
  (setq contourList nil)     ; Lista para contornos cerrados
  (setq newEntList nil)      ; Lista de nuevas entidades creadas
  ;; Obtiene el espacio modelo para agregar hatches posteriormente
  (setq modelSpace (vla-get-ModelSpace doc)) ; Espacio modelo
  
  ;;; Crear o configurar la capa Defpoints
  (setq defpointsLayer "Defpoints")
  (if (not (tblsearch "LAYER" defpointsLayer))
    (progn
      ;; Si la capa no existe, la crea con color gris (8) y tipo de línea DASHED
      (command "_.-layer" "_M" defpointsLayer "_C" "8" "" "_L" "DASHED" "" "")
      (princ "\nCapa Defpoints creada con color gris 8 y tipo de línea DASHED.")
    )
    (progn
      ;; Si la capa existe, asegura que tenga las propiedades correctas
      (command "_.-layer" "_S" defpointsLayer "_C" "8" "" "_L" "DASHED" "" "")
    )
  )
  
  ;;; Solicitar selección de entidades al usuario
  (princ "\nSeleccione líneas o polilíneas: ")
  ;; Permite seleccionar solo líneas y polilíneas ligeras
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))
  (if (not ss)
    (progn
      ;; Si no se seleccionan entidades válidas, termina el comando
      (princ "\nNo se seleccionaron entidades válidas.")
      (vla-EndUndoMark doc)
      (setvar "CMDECHO" 1)
      (setvar "PEDITACCEPT" oldPeditAccept)
      (exit)
    )
  )
  
  ;;; Solicitar distancia de offset al usuario
  (setq dist (getdist "\nDistancia de offset (centro → un lado): "))
  ;; Asegura que la distancia sea positiva y válida
  (while (or (not dist) (<= dist 0))
    (setq dist (getdist "\nDistancia debe ser positiva: "))
  )
  
  ;; Convierte el conjunto de selección en una lista de nombres de entidades
  (setq enLst (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))
  
  ;;; Procesar cada entidad seleccionada
  (foreach entName enLst
    ;; Convierte el nombre de entidad a un objeto VLA
    (setq obj (vlax-ename->vla-object entName))
    ;; Obtiene el tipo de objeto (Line o LWPolyline)
    (setq objType (vla-get-ObjectName obj))
    ;; Determina si la entidad está cerrada
    (setq isClosed (cond
                    ((= objType "AcDbLine") nil)  ; Las líneas no están cerradas
                    ((= objType "AcDbPolyline") (eq :vlax-true (vla-get-Closed obj)))
                  ))
    
    ;; Verifica si el método Offset es aplicable al objeto
    (if (vlax-method-applicable-p obj 'Offset)
      (progn
        ;; Crea offsets en ambos lados de la entidad original
        (setq off1 (vl-catch-all-apply 'vla-offset (list obj dist)))
        (setq off2 (vl-catch-all-apply 'vla-offset (list obj (- dist))))
        
        ;; Verifica si los offsets se generaron correctamente
        (if (and (not (vl-catch-all-error-p off1)) (not (vl-catch-all-error-p off2)))
          (progn
            ;; Convierte los resultados de offset a objetos VLA
            (setq off1 (car (vlax-safearray->list (vlax-variant-value off1))))
            (setq off2 (car (vlax-safearray->list (vlax-variant-value off2))))
            
            (if (and off1 off2)
              (progn
                ;; Convierte los objetos VLA a nombres de entidad
                (setq off1Ent (vlax-vla-object->ename off1))
                (setq off2Ent (vlax-vla-object->ename off2))
                ;; Asigna el color cyan a los offsets
                (vla-put-Color off1 4) ; Cyan
                (vla-put-Color off2 4) ; Cyan
                ;; Agrega los offsets a la lista de nuevas entidades
                (setq newEntList (cons off1Ent newEntList))
                (setq newEntList (cons off2Ent newEntList))
                
                ;; Si la entidad no está cerrada, crea líneas de cierre
                (if (not isClosed)
                  (progn
                    ;; Obtiene los puntos iniciales y finales de los offsets
                    (setq p1Start (vlax-curve-getStartPoint off1))
                    (setq p2Start (vlax-curve-getStartPoint off2))
                    (setq p1End (vlax-curve-getEndPoint off1))
                    (setq p2End (vlax-curve-getEndPoint off2))
                    
                    ;; Crea líneas de cierre entre los puntos iniciales y finales
                    (command "_.line" "_non" p1Start "_non" p2Start "")
                    (setq close1 (entlast))
                    (command "_.line" "_non" p1End "_non" p2End "")
                    (setq close2 (entlast))
                    
                    ;; Asigna color cyan a las líneas de cierre y las agrega a la lista
                    (if close1 (progn (vla-put-Color (vlax-ename->vla-object close1) 4) (setq newEntList (cons close1 newEntList))))
                    (if close2 (progn (vla-put-Color (vlax-ename->vla-object close2) 4) (setq newEntList (cons close2 newEntList))))
                    
                    ;; Une las entidades para formar un contorno cerrado
                    (if (and off1Ent off2Ent close1 close2)
                      (progn
                        (setq entList (list off1Ent off2Ent close1 close2))
                        (command "_.PEDIT" "_M")
                        (foreach ent entList (command ent))
                        (command "" "_J" "" "")
                        (setq joinedEnt (entlast))
                        (if joinedEnt
                          (progn
                            ;; Elimina las entidades individuales de la lista y agrega la unida
                            (setq newEntList (vl-remove off1Ent newEntList))
                            (setq newEntList (vl-remove off2Ent newEntList))
                            (setq newEntList (vl-remove off1 newEntList))
                            (setq newEntList (vl-remove off2 newEntList))
                            (setq newEntList (cons joinedEnt newEntList))
                            (vla-put-Color (vlax-ename->vla-object joinedEnt) 4)
                            (vla-put-Closed (vlax-ename->vla-object joinedEnt) :vlax-true)
                            ;; Agrega el contorno cerrado a la lista para hatch
                            (setq contourList (cons joinedEnt contourList))
                          )
                        )
                      )
                    )
                  )
                )
                ;; Mueve la entidad original a la capa Defpoints
                (vla-put-Layer obj defpointsLayer)
                (vla-put-Color obj 256) ; Color por capa (BYLAYER)
                (vla-put-Linetype obj "ByLayer")
                (princ (strcat "\nContorno creado para entidad " (itoa (vl-position entName enLst))))
              )
            )
          )
          (princ (strcat "\nError: Offset fallido para entidad " (itoa (vl-position entName enLst))))
        )
      )
      (princ (strcat "\nEntidad no compatible: " (itoa (vl-position entName enLst))))
    )
  )
  
  ;;; Aplicar hatches automáticamente a los contornos cerrados
  (if contourList
    (progn
      (initget "Sí No")
      (setq applyHatch (getkword "\n¿Aplicar hatch a los contornos cerrados? [Sí/No] <Sí>: "))
      (if (or (null applyHatch) (eq applyHatch "Sí"))
        (progn
          ;; Menú para seleccionar el patrón de hatch
          (initget "SOLID ANSI31 ANSI37 NET ANGLE")
          (setq hatchPattern (getkword "\nSeleccione patrón de hatch [SOLID/ANSI31/ANSI37/NET/ANGLE] <SOLID>: "))
          (if (not hatchPattern) (setq hatchPattern "SOLID")) ; Patrón por defecto
          
          ;; Configura la capa y el color del hatch
          (setq hatchLayer (getvar "CLAYER"))
          (setq hatchColor 4) ; Cyan
          
          ;; Aplica el hatch a cada contorno cerrado
          (foreach cont contourList
            (setq contObj (vlax-ename->vla-object cont))
            (if (eq :vlax-true (vla-get-Closed contObj))
              (progn
                ;; Crea un objeto hatch en el espacio modelo
                (setq hatchObj (vla-addhatch modelSpace 
                                             acHatchPatternTypePredefined 
                                             hatchPattern 
                                             :vlax-true 
                                             acHatchObject))
                ;; Convierte el contorno a un array seguro para ActiveX
                (setq contArray (vlax-make-safearray vlax-vbObject '(0 . 0)))
                (vlax-safearray-put-element contArray 0 contObj)
                ;; Define el contorno como bucle externo del hatch
                (vla-appendouterloop hatchObj contArray)
                ;; Configura propiedades del hatch
                (vla-put-patternscale hatchObj 1.0)
                (vla-put-patternangle hatchObj 0.0)
                (vla-put-color hatchObj hatchColor)
                (vla-put-layer hatchObj hatchLayer)
                (vla-evaluate hatchObj)
                ;; Agrega el hatch a la lista de nuevas entidades
                (setq hatchEnt (vlax-vla-object->ename hatchObj))
                (setq newEntList (cons hatchEnt newEntList))
              )
            )
          )
          (princ (strcat "\nHatch '" hatchPattern "' aplicado a " (itoa (length contourList)) " contornos."))
        )
      )
    )
  )
  
  ;;; Opción para crear un bloque con las nuevas entidades
  (if newEntList
    (progn
      (initget "Sí No")
      (setq createBlock (getkword "\n¿Crear bloque con nuevas entidades? [Sí/No] <No>: "))
      (if (and createBlock (eq createBlock "Sí"))
        (progn
          ;; Solicita el nombre del bloque y el punto base
          (setq blkName (getstring t "\nNombre del bloque: "))
          (setq basePt (getpoint "\nPunto base de inserción: "))
          (setq ssBlock (ssadd))
          ;; Agrega todas las nuevas entidades al conjunto de selección
          (foreach ent newEntList (ssadd ent ssBlock))
          (command "_.-block" blkName "_non" basePt ssBlock "")
          (princ (strcat "\nBloque '" blkName "' creado con éxito."))
        )
      )
    )
    (princ "\nAdvertencia: No se crearon nuevas entidades.")
  )
  
  ;;; Finalizar el comando
  (setvar "PEDITACCEPT" oldPeditAccept) ; Restaura el valor original
  (vla-EndUndoMark doc)                 ; Cierra la marca de deshacer
  (setvar "CMDECHO" 1)                  ; Restaura el eco de comandos
  (princ "\nProceso completado.")
  (princ)
)