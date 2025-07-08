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
                            defpointsLayer originalLayer layers defpointsLayerObj
                            oldPeditAccept entList joinedEnt contourList hatchPattern
                            applyHatch hatchEnt hatchLayer hatchColor modelSpace hatchObj)
  ;; Carga las extensiones Visual LISP para acceder a métodos ActiveX
  (vl-load-com)
  ;; Obtiene el documento activo de AutoCAD
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  
  ;;; Función de manejo de errores
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*"))
      (princ (strcat "\nError: " msg))
    )
    (if oldPeditAccept (setvar "PEDITACCEPT" oldPeditAccept))
    (if originalLayer (setvar "CLAYER" originalLayer))
    (setvar "CMDECHO" 1)
    (vla-EndUndoMark doc)
    (princ)
  )
  
  ;; Desactiva el eco de comandos y guarda la capa activa original
  (setvar "CMDECHO" 0)
  (setq originalLayer (getvar "CLAYER"))
  (vla-StartUndoMark doc)
  (setq oldPeditAccept (getvar "PEDITACCEPT"))
  (setvar "PEDITACCEPT" 1)
  (setq contourList nil)
  (setq newEntList nil)
  (setq modelSpace (vla-get-ModelSpace doc))
  
  ;;; Crear o configurar la capa Defpoints sin cambiar la capa activa
  (setq defpointsLayer "Defpoints")
  (setq layers (vla-get-Layers doc))
  (if (not (tblsearch "LAYER" defpointsLayer))
    (progn
      (setq defpointsLayerObj (vla-Add layers defpointsLayer))
      (vla-put-Color defpointsLayerObj 8)
      (vla-put-Linetype defpointsLayerObj "DASHED")
      (princ "\nCapa Defpoints creada con color gris 8 y tipo de línea DASHED.")
    )
    (princ "\nCapa Defpoints ya existe.")
  )
  
  ;;; Solicitar selección de entidades
  (princ "\nSeleccione líneas o polilíneas: ")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))
  (if (not ss)
    (progn
      (princ "\nNo se seleccionaron entidades válidas.")
      (vla-EndUndoMark doc)
      (setvar "CMDECHO" 1)
      (setvar "PEDITACCEPT" oldPeditAccept)
      (setvar "CLAYER" originalLayer)
      (exit)
    )
  )
  
  ;;; Solicitar distancia de offset
  (setq dist (getdist "\nDistancia de offset (centro → un lado): "))
  (while (or (not dist) (<= dist 0))
    (setq dist (getdist "\nDistancia debe ser positiva: "))
  )
  
  (setq enLst (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))
  
  ;;; Procesar cada entidad seleccionada
  (foreach entName enLst
    (setq obj (vlax-ename->vla-object entName))
    (setq objType (vla-get-ObjectName obj))
    (setq isClosed (cond
                    ((= objType "AcDbLine") nil)
                    ((= objType "AcDbPolyline") (eq :vlax-true (vla-get-Closed obj)))
                  ))
    
    (if (vlax-method-applicable-p obj 'Offset)
      (progn
        (setq off1 (vl-catch-all-apply 'vla-offset (list obj dist)))
        (setq off2 (vl-catch-all-apply 'vla-offset (list obj (- dist))))
        
        (if (and (not (vl-catch-all-error-p off1)) (not (vl-catch-all-error-p off2)))
          (progn
            (setq off1 (car (vlax-safearray->list (vlax-variant-value off1))))
            (setq off2 (car (vlax-safearray->list (vlax-variant-value off2))))
            
            (if (and off1 off2)
              (progn
                (setq off1Ent (vlax-vla-object->ename off1))
                (setq off2Ent (vlax-vla-object->ename off2))
                (vla-put-Color off1 4)
                (vla-put-Color off2 4)
                (vla-put-Layer off1 originalLayer)
                (vla-put-Layer off2 originalLayer)
                (setq newEntList (cons off1Ent newEntList))
                (setq newEntList (cons off2Ent newEntList))
                
                (if (not isClosed)
                  (progn
                    (setq p1Start (vlax-curve-getStartPoint off1))
                    (setq p2Start (vlax-curve-getStartPoint off2))
                    (setq p1End (vlax-curve-getEndPoint off1))
                    (setq p2End (vlax-curve-getEndPoint off2))
                    
                    (command "_.line" "_non" p1Start "_non" p2Start "")
                    (setq close1 (entlast))
                    (command "_.line" "_non" p1End "_non" p2End "")
                    (setq close2 (entlast))
                    
                    (if close1 
                      (progn 
                        (vla-put-Color (vlax-ename->vla-object close1) 4)
                        (vla-put-Layer (vlax-ename->vla-object close1) originalLayer)
                        (setq newEntList (cons close1 newEntList))
                      )
                    )
                    (if close2 
                      (progn 
                        (vla-put-Color (vlax-ename->vla-object close2) 4)
                        (vla-put-Layer (vlax-ename->vla-object close2) originalLayer)
                        (setq newEntList (cons close2 newEntList))
                      )
                    )
                    
                    (if (and off1Ent off2Ent close1 close2)
                      (progn
                        (setq entList (list off1Ent off2Ent close1 close2))
                        (command "_.PEDIT" "_M")
                        (foreach ent entList (command ent))
                        (command "" "_J" "" "")
                        (setq joinedEnt (entlast))
                        (if joinedEnt
                          (progn
                            (setq newEntList (vl-remove off1Ent newEntList))
                            (setq newEntList (vl-remove off2Ent newEntList))
                            (setq newEntList (vl-remove close1 newEntList))
                            (setq newEntList (vl-remove close2 newEntList))
                            (setq newEntList (cons joinedEnt newEntList))
                            (vla-put-Color (vlax-ename->vla-object joinedEnt) 4)
                            (vla-put-Layer (vlax-ename->vla-object joinedEnt) originalLayer)
                            (vla-put-Closed (vlax-ename->vla-object joinedEnt) :vlax-true)
                            (setq contourList (cons joinedEnt contourList))
                          )
                        )
                      )
                    )
                  )
                )
                (vla-put-Layer obj defpointsLayer)
                (vla-put-Color obj 256)
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
  
  ;;; Aplicar hatches
  (if contourList
    (progn
      (initget "Sí No")
      (setq applyHatch (getkword "\n¿Aplicar hatch a los contornos cerrados? [Sí/No] <Sí>: "))
      (if (or (null applyHatch) (eq applyHatch "Sí"))
        (progn
          (initget "SOLID ANSI31 ANSI37 NET ANGLE")
          (setq hatchPattern (getkword "\nSeleccione patrón de hatch [SOLID/ANSI31/ANSI37/NET/ANGLE] <SOLID>: "))
          (if (not hatchPattern) (setq hatchPattern "SOLID"))
          
          (setq hatchLayer originalLayer)
          (setq hatchColor 4)
          
          (foreach cont contourList
            (setq contObj (vlax-ename->vla-object cont))
            (if (eq :vlax-true (vla-get-Closed contObj))
              (progn
                (setq hatchObj (vla-addhatch modelSpace 
                                             acHatchPatternTypePredefined 
                                             hatchPattern 
                                             :vlax-true 
                                             acHatchObject))
                (setq contArray (vlax-make-safearray vlax-vbObject '(0 . 0)))
                (vlax-safearray-put-element contArray 0 contObj)
                (vla-appendouterloop hatchObj contArray)
                (vla-put-patternscale hatchObj 1.0)
                (vla-put-patternangle hatchObj 0.0)
                (vla-put-color hatchObj hatchColor)
                (vla-put-layer hatchObj hatchLayer)
                (vla-evaluate hatchObj)
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
  
  ;;; Opción para crear un bloque
  (if newEntList
    (progn
      (initget "Sí No")
      (setq createBlock (getkword "\n¿Crear bloque con nuevas entidades? [Sí/No] <No>: "))
      (if (and createBlock (eq createBlock "Sí"))
        (progn
          (setq blkName (getstring t "\nNombre del bloque: "))
          (setq basePt (getpoint "\nPunto base de inserción: "))
          (setq ssBlock (ssadd))
          (foreach ent newEntList (ssadd ent ssBlock))
          (command "_.-block" blkName "_non" basePt ssBlock "")
          (princ (strcat "\nBloque '" blkName "' creado con éxito."))
        )
      )
    )
    (princ "\nAdvertencia: No se crearon nuevas entidades.")
  )
  
  ;;; Finalizar el comando
  (setvar "PEDITACCEPT" oldPeditAccept)
  (vla-EndUndoMark doc)
  (setvar "CLAYER" originalLayer)
  (setvar "CMDECHO" 1)
  (princ "\nProceso completado.")
  (princ)
)
