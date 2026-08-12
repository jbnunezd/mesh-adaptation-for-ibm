#----------------------------------------------------------------------------------------------------------------------#
# Create a list of subfolders
#----------------------------------------------------------------------------------------------------------------------#
MACRO(SUBFOLDERLIST subfolderlist currentfolder)
  FILE(GLOB subfolders RELATIVE ${currentfolder} ${currentfolder}/*)
  SET(folderlist "")
  FOREACH(folder ${subfolders})
    IF(IS_DIRECTORY ${currentfolder}/${folder})
      LIST(APPEND folderlist ${folder})
    ENDIF()
  ENDFOREACH()
  SET(${subfolderlist} ${folderlist})
ENDMACRO()
#----------------------------------------------------------------------------------------------------------------------#
