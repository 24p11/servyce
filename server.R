
options(shiny.maxRequestSize=10000*1024^2) 

function(input, output, session) {
  
  score <<- 0
  
  url <- a("à cette page", href = "http://sls-dbim02.sls.aphp.fr:3838/4115983/imports/R/")
  output$urlimports <- renderUI({tagList("Une interface de production des données au bon format est disponible ", url)})
  
  observeEvent( input$build, {
    
    courant <- input$courant ; consol <- input$consol ; nonconsol <- input$nonconsol ; tarifsant <- input$tarifsant 
    if (is.null(courant) | is.null(consol) | is.null(nonconsol) | is.null(tarifsant)) {return(NULL)}

    withProgress( message = "Chargement", value = 0, {
      
      incProgress(0, detail = paste("fichiers R 1/4"))
      
      #load courant
      # load(paste0("/mnt/commons/Groupe_de_travail/GH_PMSI/DATA/WD/Rpmsi_pmeasyr_", anno, formatC(mese, width = 2, format = "d", flag = "0"), ".RData"))
      # load("/mnt/commons/Groupe_de_travail/GH_PMSI/DATA/Remontees/temp/test_shiny/courant_all/pmsi_2019_M6_2019-08-01.RData")
      load(courant[[1,'datapath']])

      anno <<- as.numeric(max(pmsidata$rsa$ansor))
      mese <<- as.numeric(max(pmsidata$rsa$moissor))
      anonconsol <<- anno-1
      mnonconsol <<- mese

      assign(paste0("rsa_", anno, formatC(mese, width = 2, format = "d", flag = "0")), pmsidata$rsa)
      assign(paste0("rum_", anno, formatC(mese, width = 2, format = "d", flag = "0")), pmsidata$rum)
      assign(paste0("rsa_v_", anno, formatC(mese, width = 2, format = "d", flag = "0")), pmsidata$rsa_v)
      assign(paste0("rum_v_", anno, formatC(mese, width = 2, format = "d", flag = "0")), pmsidata$rum_v)
      assign(paste0("pmctmono_", anno, formatC(mese, width = 2, format = "d", flag = "0")), pmsidata$pmctmono %>% dplyr::mutate(ansor=as.character(anno)))
      assign(paste0("diagnostics_", anno, formatC(mese, width = 2, format = "d", flag = "0")), pmsidata$diagnostics)
      
      incProgress(0.2, detail = paste("fichiers R 2/4"))
      
      #load consolidés
      # load(paste0("/mnt/commons/Groupe_de_travail/GH_PMSI/DATA/WD/Rpmsi_pmeasyr_", anno-1, "12.RData"))
      # load("/mnt/commons/Groupe_de_travail/GH_PMSI/DATA/Remontees/temp/test_shiny/pmsi_2018_M12_2019-07-31.RData")
      load(consol[[1,'datapath']])
      
      assign(paste0("rsa_", anno-1, "12"), pmsidata$rsa)
      assign(paste0("rum_", anno-1, "12"), pmsidata$rum)
      assign(paste0("rsa_v_", anno-1, "12"), pmsidata$rsa_v)
      assign(paste0("rum_v_", anno-1, "12"), pmsidata$rum_v)
      assign(paste0("pmctmono_", anno-1, "12"), pmsidata$pmctmono %>% dplyr::mutate(ansor=as.character(anno-1)))
      assign(paste0("diagnostics_", anno-1, "12"), pmsidata$diagnostics)
      
      incProgress(0.2, detail = paste("fichiers R 3/4"))
      
      #load non consolidé
      # load(paste0("/mnt/commons/Groupe_de_travail/GH_PMSI/DATA/WD/Rpmsi_pmeasyr_", anno-1, formatC(mese, width = 2, format = "d", flag = "0"), ".RData"))
      # load("/mnt/commons/Groupe_de_travail/GH_PMSI/DATA/Remontees/temp/test_shiny/pmsi_2018_M6_2019-08-01.RData")
      load(nonconsol[[1,'datapath']])

      assign(paste0("rsa_", anno-1, formatC(mese, width = 2, format = "d", flag = "0")), pmsidata$rsa)
      assign(paste0("rum_", anno-1, formatC(mese, width = 2, format = "d", flag = "0")), pmsidata$rum)
      assign(paste0("rsa_v_", anno-1, formatC(mese, width = 2, format = "d", flag = "0")), pmsidata$rsa_v)
      assign(paste0("rum_v_", anno-1, formatC(mese, width = 2, format = "d", flag = "0")), pmsidata$rum_v)
      assign(paste0("pmctmono_", anno-1, formatC(mese, width = 2, format = "d", flag = "0")), pmsidata$pmctmono)
      
      incProgress(0.2, detail = paste("fichiers R 4/4"))
      
      #load tarifs ant
      # load(paste0("/mnt/commons/Groupe_de_travail/GH_PMSI/DATA/WD/Rpmsi_pmeasyr_tarifs_anterieurs_", anno, formatC(mese, width = 2, format = "d", flag = "0"), ".RData"))
      # load("/mnt/commons/Groupe_de_travail/GH_PMSI/DATA/Remontees/temp/test_shiny/courant_all/pmsi_tarifs_anterieurs_2019_M6_2019-08-01.RData")
      load(tarifsant[[1,'datapath']])

      assign(paste0("rum_v_tarifs_anterieurs_", anno, formatC(mese, width = 2, format = "d", flag = "0")), pmsidataant$rum_v_tarifs_anterieurs)
      
      incProgress(0.2, detail = paste("formatation"))
      
      rsa <- dplyr::left_join(bind_rows(get(paste0("rsa_", anno, formatC(mese, width = 2, format = "d", flag = "0"))), get(paste0("rsa_", anno-1, "12"))), bind_rows(get(paste0("rsa_v_", anno, formatC(mese, width = 2, format = "d", flag = "0"))), get(paste0("rsa_v_", anno-1, "12"))))
      rum <- dplyr::left_join(bind_rows(get(paste0("rum_", anno, formatC(mese, width = 2, format = "d", flag = "0"))), get(paste0("rum_", anno-1, "12"))), bind_rows(get(paste0("rum_v_", anno, formatC(mese, width = 2, format = "d", flag = "0"))), get(paste0("rum_v_", anno-1, "12"))))
      pmctmono <<- bind_rows(get(paste0("pmctmono_", anno, formatC(mese, width = 2, format = "d", flag = "0"))), get(paste0("pmctmono_", anno-1, "12")))
      diagnostics <<- bind_rows(get(paste0("diagnostics_", anno, formatC(mese, width = 2, format = "d", flag = "0"))), get(paste0("diagnostics_", anno-1, "12")))
      
      pmctmono_nonconsol <- get(paste0("pmctmono_", anno-1, formatC(mese, width = 2, format = "d", flag = "0")))
      rsa_nonconsol <- dplyr::left_join(get(paste0("rsa_", anno-1, formatC(mese, width = 2, format = "d", flag = "0"))), get(paste0("rsa_v_", anno-1, formatC(mese, width = 2, format = "d", flag = "0"))))
      rum_nonconsol <- dplyr::left_join(get(paste0("rum_", anno-1, formatC(mese, width = 2, format = "d", flag = "0"))), get(paste0("rum_v_", anno-1, formatC(mese, width = 2, format = "d", flag = "0"))))
      
      rum_tarifsante <- dplyr::left_join(get(paste0("rum_", anno, formatC(mese, width = 2, format = "d", flag = "0"))), get(paste0("rum_v_tarifs_anterieurs_", anno, formatC(mese, width = 2, format = "d", flag = "0"))))
      # rum_tarifsante <- dplyr::left_join(get(paste0("rum_tarifs_anterieurs_", anno, formatC(mese, width = 2, format = "d", flag = "0"))), get(paste0("rum_valo_tarifs_anterieurs_", anno, formatC(mese, width = 2, format = "d", flag = "0"))))
      # pmctmono_tarifsante <- get(paste0("pmctmono_tarifs_anterieurs_", anno, formatC(mese, width = 2, format = "d", flag = "0")))
      
      rm(list = eval(paste0("rsa_", anno, formatC(mese, width = 2, format = "d", flag = "0"))))
      rm(list = eval(paste0("rum_", anno, formatC(mese, width = 2, format = "d", flag = "0"))))
      rm(list = eval(paste0("rum_v_", anno, formatC(mese, width = 2, format = "d", flag = "0"))))
      rm(list = eval(paste0("diagnostics_", anno, formatC(mese, width = 2, format = "d", flag = "0"))))
      rm(list = eval(paste0("pmctmono_", anno, formatC(mese, width = 2, format = "d", flag = "0"))))
      
      rm(list = eval(paste0("rsa_", anno-1, "12")))
      rm(list = eval(paste0("rum_", anno-1, "12")))
      rm(list = eval(paste0("rum_v_", anno-1, "12")))
      rm(list = eval(paste0("diagnostics_", anno-1, "12")))
      rm(list = eval(paste0("pmctmono_", anno-1, "12")))
      
      rm(list = eval(paste0("rsa_", anno-1, formatC(mese, width = 2, format = "d", flag = "0"))))
      rm(list = eval(paste0("rum_", anno-1, formatC(mese, width = 2, format = "d", flag = "0"))))
      rm(list = eval(paste0("rum_v_", anno-1, formatC(mese, width = 2, format = "d", flag = "0"))))
      rm(list = eval(paste0("diagnostics_", anno-1, formatC(mese, width = 2, format = "d", flag = "0"))))
      rm(list = eval(paste0("pmctmono_", anno-1, formatC(mese, width = 2, format = "d", flag = "0"))))
      
      rm(list = eval(paste0("rum_v_tarifs_anterieurs_", anno, formatC(mese, width = 2, format = "d", flag = "0"))))

      incProgress(0.05, detail = paste("fichiers annexes"))
      
      # load structures au cas où structures non intégrées au préalable
      # fichier_structure <- readxl::read_excel('/mnt/commons/Groupe_de_travail/GH_PMSI/DATA/Structures/structures.xlsx', col_types = c( "text" , "text" , "text" , "text" ,"text", "text" , "text" , "text" , "text" ) )
      # names(fichier_structure) <- c('nofiness', 'hopital', 'cdurm', 'uma_locale', 'uma_locale2', 'libelle_um', 'service', 'regroupement1', 'pole')
      
      # load referentiels des DMS nationales:
      dms1 <- nomensland::get_table('ghm_dms_nationales') %>% dplyr::filter(ghs!="") %>% dplyr::bind_rows(., dplyr::filter(nomensland::get_table('ghm_dms_nationales') %>% dplyr::filter(ghs!="")  %>% dplyr::filter(anseqta==anno-1) %>% dplyr::mutate(anseqta=as.character(anno))))
      dms2 <- nomensland::get_table('ghm_dms_nationales') %>% dplyr::filter(ghs=="") %>% dplyr::bind_rows(., dplyr::filter(nomensland::get_table('ghm_dms_nationales') %>% dplyr::filter(ghs=="")  %>% dplyr::filter(anseqta==anno-1) %>% dplyr::mutate(anseqta=as.character(anno))))
      
      # load referentiel regroupements:
      reg1 <- nomensland::get_table('ghm_ghm_regroupement') %>% dplyr::mutate(racine=substr(ghm,1,5))
      reg2 <- dplyr::bind_rows(nomensland::get_table('ghm_rghm_regroupement'), nomensland::get_table('ghm_rghm_regroupement') %>% dplyr::filter(anseqta=="2017") %>% dplyr::mutate(anseqta="2018"))
      
      # load referentiel tarifs:
      tarifs <<- nomensland::get_table('tarifs_mco_ghs') 	
      
      incProgress(0.1, detail = paste("création des variables"))
      
      # objet giacimento
      # giacimentoload <<- rsa_nonconsol %>% dplyr::mutate(racine=substr(ghm, 1, 5), severite=substr(ghm, 6, 6)) %>% dplyr::select(nas, ghm, racine, severite, rec_base)

      #mutate rums and rsa
      # ICI trop de lignes en sortie: rsa <- rsa_bckp %>% dplyr::filter(ansor>=anno-2) %>% dplyr::mutate(ghs=noghs) %>% dplyr::left_join(., dms1, suffix=c('','_temp'), by=c('ghs', 'anseqta', 'rsavclass')) %>% dplyr::select(-ghm_temp, -ghs) %>% dplyr::left_join(., dms2, suffix=c('', '_temp'), by=c('ghm', 'anseqta', 'rsavclass')) %>% dplyr::mutate(dms_n=ifelse(is.na(dms_n), dms_n_temp, dms_n), borne_basse=ifelse(is.na(borne_basse), borne_basse_temp, borne_basse), borne_haute=ifelse(is.na(borne_haute), borne_haute_temp, borne_haute), ip=duree/dms_n, racine=paste0(rsacmd, rsatype, rsanum)) %>% dplyr::select(-dms_n_temp, -borne_basse_temp, -borne_haute_temp) %>% dplyr::left_join(., reg2)
      rsaload <<- rsa %>% dplyr::filter(ansor>=anno-2) %>% dplyr::mutate(ghs=noghs) %>% dplyr::left_join(., dms1, suffix=c('','_temp')) %>% dplyr::select(-ghs) %>% dplyr::left_join(., dms2, suffix=c('', '_temp'), by=c('ghm', 'anseqta', 'rsavclass')) %>% dplyr::mutate(dms_n=ifelse(is.na(dms_n), dms_n_temp, dms_n), borne_basse=ifelse(is.na(borne_basse), borne_basse_temp, borne_basse), borne_haute=ifelse(is.na(borne_haute), borne_haute_temp, borne_haute), ip=duree/dms_n, racine=paste0(rsacmd, rsatype, rsanum)) %>% dplyr::select(-dms_n_temp, -borne_basse_temp, -borne_haute_temp) %>% dplyr::left_join(., reg2)
      
      #Pour shiny_app:
      rumload <<- rum %>% dplyr::left_join(., rsaload %>% dplyr::select(cle_rsa, nas, norss, ip, nbrum_sej=nbrum, duree_sej=duree, racine_sej=racine, libelle_racine_sej=libelle_racine, da_sej=da, libelle_da_sej=libelle_da, gp_cas_sej=gp_cas, libelle_gp_cas_sej=libelle_gp_cas, ga_sej=ga, libelle_ga_sej=libelle_ga, da_gp_sej=da_gp, da_gp_ga_sej=da_gp_ga, aso_sej=aso)) %>% dplyr::left_join(., rum_tarifsante %>% dplyr::select(nas, norum, valotime, valopmctmono, valopmctmonotime1, valopmctmonotime2), suffix=c("","_tarifsante"), by=c('nas', 'norum')) %>% dplyr::left_join(., rum_nonconsol %>% dplyr::filter(ansor==anno-1) %>% dplyr::select(nas, norum, valotime, valopmctmono, valopmctmonotime1, valopmctmonotime2), suffix=c("","_nonconsol"), by=c('nas', 'norum')) %>% dplyr::group_by(nas) %>% dplyr::mutate(ip_repa=ip*(dureesejpart+1)/(sum(dureesejpart+1)), LIBUM=paste0(uma_locale2, " - ",libelle_um)) %>% dplyr::ungroup() %>% dplyr::select(-ip) %>% dplyr::rename(ansor_sej=ansor, moissor_sej=moissor)
      
      structures <<- rumload  %>% dplyr::mutate(services=libelle_um, value=uma_locale2, label=paste0(uma_locale2, " - ", libelle_um)) %>% dplyr::select(services, value, label) %>% unique()
      
      score <<- 0
      
      updateSelectizeInput(session, 'uma', choices = structures, server = TRUE)
      
      # save(anno, mese, anonconsol, mnonconsol, pmctmono, diagnostics, giacimentoload, rsaload, rumload, structures, tarifs, score, file = 'temp/servyce.Rdata')
      save(anno, mese, anonconsol, mnonconsol, pmctmono, diagnostics, rsaload, rumload, structures, tarifs, score, file = 'temp/servyce.Rdata')
      
    } )
  } )
  

  observeEvent(input$load, {
    
    withProgress( message = "Chargement", value = 0, {
      
        incProgress(0.1, detail = paste("données préconstruites")) 
        load('temp/servyce.Rdata') 
        incProgress(0.7, detail = paste("création des variables")) 
        anno <<- anno
        mese <<- mese
        anonconsol <<- anonconsol
        mnonconsol <<- mnonconsol
        rumload <<- rumload 
        rsaload <<- rsaload 
        # giacimentoload <<- giacimentoload 
        tarifs <<- tarifs 
        pmctmono <<- pmctmono 
        diagnostics <<- diagnostics
        anonconsol <<- anonconsol 
        mnonconsol <<- mnonconsol 
        structures <<- structures
        score <<- score 
        # load <<- 1

        updateSelectizeInput(session, 'uma', choices = structures, server = TRUE) 
      
    })
  } )
  
  
  observeEvent(input$scores, {
    
    withProgress( message = "Calcul", value = 0, {
      
      if(score==0) {
      incProgress(0.05, detail = paste("score de Charlson"))
      
      # calcul de scores de comorbidité (https://cran.r-project.org/web/packages/comorbidity/vignettes/comorbidityscores.html)
      charl <- comorbidity(x = diagnostics, id = "nas", code = "diag", score = "charlson", icd = "icd10", assign0 = FALSE) %>% dplyr::select(nas, charl_score=score, charl_wscore=wscore, charl_index=index, charl_windex=windex)
      
      incProgress(0.5, detail = paste("score de Elixhauser"))
      
      elix <- comorbidity(x = diagnostics, id = "nas", code = "diag", score = "elixhauser", icd = "icd10", assign0 = FALSE) %>% dplyr::select(nas, elix_score=score, elix_wscore_ahrq=wscore_ahrq, elix_wscore_vw=wscore_vw, elix_index=index, elix_windex_ahrq=windex_ahrq, elix_windex_vw=windex_vw)
      
      scores <- dplyr::left_join(charl, elix)
      
      incProgress(0.4, detail = paste("intégration"))
      
      rsaload <<- dplyr::left_join(rsaload, scores) 
      
      score <<- 1

      # save(anno, mese, anonconsol, mnonconsol, pmctmono, diagnostics, giacimentoload, rsaload, rumload, structures, tarifs, score, file = 'temp/servyce.Rdata')
      save(anno, mese, anonconsol, mnonconsol, pmctmono, diagnostics, rsaload, rumload, structures, tarifs, score, file = 'temp/servyce.Rdata')
      
      } else {NULL}
    })
  } )
  
  observeEvent(input$calcul, {
    
    withProgress( message = "Calcul", value = 0, {
      
      incProgress(0.3, detail = paste("selection du regroupement"))
      
      unita <<- input$uma
      temp1 <- structures %>% dplyr::filter(value %in% unita) %>% dplyr::select(label)
      unitemed <<- paste0(temp1$label, collapse=" & ")
      
      # naselect <<- rum %>% dplyr::filter(uma_locale2 %in% unita) %>% dplyr::select(nas)  
      if(input$ghmfilt2 == "") {
        naselect <<- rumload %>% dplyr::filter(uma_locale2 %in% unita, str_detect(cdghm, input$ghmfilt1)) %>% dplyr::select(nas)
      } else {
        naselect <<- rumload %>% dplyr::filter(uma_locale2 %in% unita, str_detect(cdghm, input$ghmfilt1), !str_detect(cdghm, input$ghmfilt2)) %>% dplyr::select(nas)
      }

      # giacimento1 <- giacimentoload %>% dplyr::filter(nas %in% naselect$nas) %>% dplyr::group_by(racine) %>% dplyr::mutate(nrac_giac=length(ghm), recrac_giac=sum(rec_base)) %>% dplyr::group_by(ghm) %>% dplyr::mutate(nghm_giac=length(ghm), recghm_giac=sum(rec_base)) %>% dplyr::select(-nas) %>% dplyr::distinct(ghm, nrac_giac, nghm_giac, recrac_giac, recghm_giac) %>% dplyr::mutate(propghm_giac=nghm_giac/nrac_giac, proprec_giac=recghm_giac/recrac_giac) 
      # giacimento2 <- rsaload %>% dplyr::filter(nas %in% naselect$nas, ansor==anno) %>% dplyr::group_by(racine) %>% dplyr::mutate(nrac=length(ghm), recrac=sum(rec_base)) %>% dplyr::group_by(ghm) %>% dplyr::mutate(nghm=length(ghm), recghm=sum(rec_base)) %>% dplyr::select(-nas) %>% dplyr::distinct(ghm, nrac, nghm, recrac, recghm) %>% dplyr::mutate(propghm=nghm/nrac, proprec=recghm/recrac)
      # giacimento <<- dplyr::left_join(giacimento2, giacimento1) %>% dplyr::mutate(theoriq=ifelse(is.na(proprec_giac), recghm, proprec_giac*recrac), diff=ifelse(is.na(proprec_giac), 0, recghm-proprec_giac*recrac))

      incProgress(0.4, detail = paste("finalisation"))
      
      rum <<- rumload %>% dplyr::filter(nas %in% naselect$nas) %>% dplyr::mutate(LIBUM=if_else(uma_locale2 %in% unita, unitemed, paste0(uma_locale2, " - ", libelle_um)))
      rsa <<- rsaload %>% dplyr::filter(nas %in% naselect$nas)
      
      output$listuma <- renderText({paste0("Calcul ok pour ", unitemed, "à M", mese, " de ", anno, " avec GHM inclus = ", input$ghmfilt1, " et GHM exclus = ", input$ghmfilt2)})
      
      #Calcul objet incluant recettes théoriques (par racines)
      
      giac1 <- dplyr::filter(rum, as.numeric(moissor_sej)<=mese, as.numeric(ansor_sej)==anno-1, LIBUM %in% unitemed) %>%
        dplyr::group_by(nas) %>% dplyr::summarise(ghm=first(cdghm), coefpmctmonotime1=sum(coefpmctmonotime1), valopmctmonotime1=sum(valopmctmonotime1), valopmctmonotime1_nonconsol=sum(valopmctmonotime1_nonconsol), rec_sup_repa=sum(rec_sup_repa)) %>% dplyr::ungroup(.) %>%
        dplyr::left_join(.,  dplyr::left_join(rsa %>% dplyr::mutate(rec_exbh=rec_exh+rec_exb) %>% dplyr::select(nas, ghs=noghs, rec_base, rec_exbh), dplyr::filter(tarifs, anseqta==as.character(anno)) %>% dplyr::select(ghs, tarif_base) ) ) %>% dplyr::mutate(racine=substr(ghm, 1, 5), valo_base=coefpmctmonotime1*rec_base, valo_exbh=coefpmctmonotime1*rec_exbh, diffconsol=valopmctmonotime1-valopmctmonotime1_nonconsol) %>%
        dplyr::group_by(racine) %>%
        dplyr::mutate(nrac=length(ghs), recrac_consol=sum(valo_base), recrac_exbh_consol=sum(valo_exbh), recrac_diffconsol=sum(diffconsol), recracsupconsol=sum(rec_sup_repa)) %>%
        dplyr::group_by(ghs) %>%
        dplyr::mutate(nghs=length(ghs), repanum=length(ghs)/nrac*tarif_base) %>% 
        ungroup(.) %>%
        dplyr::distinct(ghm, ghs, .keep_all=TRUE) %>%
        dplyr::group_by(racine) %>%
        dplyr::summarise(repanum_rac=sum(repanum), nrac_consol=first(nrac), recrac_consol=first(recrac_consol), recrac_exbh_consol=first(recrac_exbh_consol), evol_consol=first(recrac_diffconsol), recracsup_consol=first(recracsupconsol))
      
      giac2 <- dplyr::left_join(dplyr::filter(rum, ansor_sej==anno, LIBUM %in% unitemed) %>% dplyr::group_by(nas) %>% dplyr::summarise(ghm=first(cdghm), coefpmctmonotime1=sum(coefpmctmonotime1), valopmctmonotime1=sum(valopmctmonotime1), valopmctmonotime1_tarifsante=sum(valopmctmonotime1_tarifsante), rec_sup_repa=sum(rec_sup_repa)) %>% dplyr::ungroup(.), rsa  %>% dplyr::mutate(rec_exbh=rec_exh+rec_exb) %>% dplyr::select(nas, ghs=noghs, rec_base, rec_exbh) ) %>% 
        dplyr::mutate(racine=substr(ghm, 1, 5), valo_base=coefpmctmonotime1*rec_base, valo_exbh=coefpmctmonotime1*rec_exbh, difftarifsante=valopmctmonotime1_tarifsante-valopmctmonotime1) %>% dplyr::group_by(racine) %>%
        dplyr::summarise(nrac=sum(coefpmctmonotime1), nracreel=length(ghs), recrac=sum(valo_base), recrac_exbh=sum(valo_exbh), recrac_difftarifsante=sum(difftarifsante), recracsup=sum(rec_sup_repa)) %>% ungroup(.)
      
      giac <<- dplyr::full_join(giac1, giac2) %>%
        dplyr::mutate_at(., vars(repanum_rac, nrac_consol, recrac_consol, recrac_exbh_consol, evol_consol, recracsup_consol), ~ifelse(is.na(.), 0, .)) %>%  
        dplyr::mutate(theoriq_num=repanum_rac*nrac, diff_rec=(recrac+recrac_exbh+recracsup)-(recrac_consol+recrac_exbh_consol+recracsup_consol), diff_rec_base=recrac-recrac_consol, diff_rec_exbh=recrac_exbh-recrac_exbh_consol, diff_rec_supp=recracsup-recracsup_consol, diff_tarifsante=recrac_difftarifsante, diff_consol=evol_consol, diff_theoriq=recrac-theoriq_num, diff_reste=diff_rec_base+diff_rec_exbh+diff_rec_supp+diff_tarifsante+diff_theoriq)
      
    })
  } )
  
  
  observeEvent(eventExpr = input$calcul, ignoreInit = TRUE, {
    
    output$activ1 <- DT::renderDT( {
      
      rsa %>% dplyr::filter(as.numeric(moissor)<=mese, as.numeric(ansor)>=anno-1) %>% dplyr::group_by("Année de sortie"=ansor, "Mois de sortie"=moissor) %>% dplyr::summarise("Nbr de séjours"=n_distinct(nas), "Nb de séjours monorums"=n_distinct(nas[nbrum==1]), "IP moyen"=round(mean(ip, na.rm=TRUE),3))
      
    }, caption=paste("Descriptif des séjours passés par l'unité", unitemed), rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
    
    output$activ2 <- DT::renderDT( {
      
      twosteps <- function (x, var, niv1='ansor_sej', niv2='moissor_sej', niv3='nas', niv4='LIBUM', fct=list(mean=mean, median=median, rec=sum, nbrap=n_distinct)) {
        dplyr::group_by(x, !!sym(niv1), !!sym(niv2), !!sym(niv3), !!sym(niv4)) %>% dplyr::summarise(s=sum(!!sym(var), na.rm=TRUE)) %>% dplyr::group_by(!!sym(niv4), !!sym(niv1), !!sym(niv2)) %>% dplyr::summarise_at('s', fct, na.rm = TRUE) %>% dplyr::rename_at(vars(names(.) %>% tail(length(fct))), ~ paste0(var,"_",.))
      }
      
      tempip <- twosteps(x=rum %>% dplyr::filter(uma_locale2 %in% unita, as.numeric(moissor_sej)<=mese), var='ip_repa', niv2='', fct=list(mean=mean, median=median))
      tempdmr <- twosteps(x=rum %>% dplyr::filter(uma_locale2 %in% unita, as.numeric(moissor_sej)<=mese), var='dureesejpart', niv2='', fct=list(mean=mean, median=median))
      tempdms <- twosteps(x=rum %>% dplyr::filter(uma_locale2 %in% unita, as.numeric(moissor_sej)<=mese), var='duree_sej', niv2='', fct=list(mean=mean, median=median))
      dplyr::left_join(tempdms, tempdmr) %>% dplyr::left_join(., tempip) %>% dplyr::ungroup() %>% dplyr::select(-LIBUM) %>% dplyr::rename("Année de sortie"=ansor_sej, DMS=duree_sej_mean, DMedianeS=duree_sej_median, DMR=dureesejpart_mean, DMedianeR=dureesejpart_median, "IP moyen"=ip_repa_mean, "IP median"=ip_repa_median) %>% dplyr::mutate_at(vars(-1), ~round(., 2))
      
    }, caption=paste0("Durée des séjours passés par l'unité ", unitemed," par année jusqu'à M", mese), rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
    
    output$activ3 <- DT::renderDT( {
      
      rsa %>% dplyr::filter(as.numeric(moissor)<=mese, as.numeric(ansor)>=anno-1) %>% dplyr::mutate(typehosp = case_when(nbseance > 0 ~ "Séances", duree == 0 ~ "Sans nuité hors séances", duree > 0 & duree <= 2 ~ "HC <= 2 jours", duree > 2 ~ "HC > 2 jours", TRUE ~"ERREUR") ) %>% dplyr::group_by(Année=ansor, "Type d'hospitalisation"=typehosp) %>% dplyr::summarise("Nbr de séjours"=n_distinct(nas))
      
    }, caption=paste0("Typologie des séjours selon la durée passés par ", unitemed," par année jusqu'à M", mese), rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
    
    output$activ4 <- renderPlotly({
      
      temp1 <- rsa %>% dplyr::filter(as.numeric(ansor)>=anno-1) %>% dplyr::mutate(ghs=noghs, typehosp = case_when(nbseance > 0 ~ "Séances", duree == 0 ~ "Sans nuité hors séances", duree > 0 & duree <= 2 ~ "HC <= 2 jours", duree > 2 ~ "HC > 2 jours", TRUE ~"ERREUR") ) %>% dplyr::filter(ansor>=anno-1) %>% dplyr::mutate(temps=as.factor(paste0(ansor, " - M", moissor))) %>% dplyr::group_by(typehosp, temps) %>% dplyr::summarise(nbr_sejours=n_distinct(nas)) %>% dplyr::ungroup()
      ggplotly(ggplot(temp1, (aes(x = temps,  y = nbr_sejours, group = typehosp, fill = typehosp))) + geom_area() + theme(axis.text.x = element_text(angle = 66)), tooltip = "y" )
      
    })    
    
    output$activ5 <- DT::renderDT( {
      
      rsa %>% dplyr::filter(as.numeric(moissor)<=mese, as.numeric(ansor)>=anno-1) %>% dplyr::mutate(typehosp = case_when(nbseance > 0 ~ "HP en seances", duree > 0 & !gpcompx %in% c("T", "J") ~ "HC", duree > 0 & gpcompx == "T" ~ "HC de très courte durée", duree == 0 & gpcompx == "J" ~ "HC pour ambulatoire", duree > 0 ~ "HC", duree == 0 ~ "HP", TRUE ~"ERREUR") ) %>% dplyr::group_by(Année=ansor, "Type d'hospitalisation"=typehosp) %>% dplyr::summarise("Nbr de séjours"=n_distinct(nas))
      
    }, caption=paste0("Typologie des séjours passés par l'unité passés par ", unitemed," par année jusqu'à M", mese), rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
    
    output$activ6 <- renderPlotly({
      
      temp1 <- rsa %>% dplyr::filter(as.numeric(ansor)>=anno-1) %>% dplyr::mutate(ghs=noghs, typehosp = case_when(nbseance > 0 ~ "HP en seances", duree > 0 & !gpcompx %in% c("T", "J") ~ "HC", duree > 0 & gpcompx == "T" ~ "HC de très courte durée", duree == 0 & gpcompx == "J" ~ "HC pour ambulatoire", duree > 0 ~ "HC", duree == 0 ~ "HP", TRUE ~"ERREUR") ) %>% dplyr::filter(ansor>=anno-1) %>% dplyr::mutate(temps=as.factor(paste0(ansor, " - M", moissor))) %>% dplyr::group_by(typehosp, temps) %>% dplyr::summarise(nbr_sejours=n_distinct(nas)) %>% dplyr::ungroup()
      ggplotly(ggplot(temp1, (aes(x = temps,  y = nbr_sejours, group = typehosp, fill = typehosp))) + geom_area() + theme(axis.text.x = element_text(angle = 66)), tooltip = "y" )
      
    })    
    
    output$activ7 <- renderPlotly({
      
      temp1 <- rsa %>% dplyr::mutate(ghs=noghs) %>% dplyr::filter(ansor>=anno-1) %>% dplyr::mutate(type=substr(ghm,3,3), temps=paste0(ansor, " - M", moissor)) %>% dplyr::group_by(type, temps) %>% dplyr::summarise(nbr_sejours=n_distinct(nas)) %>% ungroup()
      ggplotly(ggplot(temp1, (aes(x = temps,  y = nbr_sejours, group = type, fill = type))) + geom_area() + theme(axis.text.x = element_text(angle = 66)), tooltip = "y" )

    })
    
    output$activ8 <- renderPlotly({
      
      temp1 <- rsa %>% dplyr::mutate(ghs=noghs) %>% dplyr::filter(ansor>=anno-1) %>% dplyr::mutate(severite=substr(ghm,6,6), temps=paste0(ansor, " - M", moissor)) %>% dplyr::group_by(severite, temps) %>% dplyr::summarise(nbr_sejours=n_distinct(nas)) %>% ungroup()
      ggplotly(ggplot(temp1, (aes(x = temps,  y = nbr_sejours, group = severite, fill = severite))) + geom_area() + theme(axis.text.x = element_text(angle = 66)), tooltip = "y" )
      
    })

    output$activ9 <- renderPlotly({
      
      if(score==1) {
      temp1 <- rsa %>% dplyr::filter(ansor>=anno-1) %>% dplyr::mutate(temps=paste0(ansor, " - M", moissor), gpcompx=as.integer(gpcompx), max_charl_score=max(charl_score), max_elix_score=max(charl_score), max_gpcompx=max(gpcompx, na.rm=TRUE)) %>% dplyr::group_by(temps) %>% dplyr::summarise(meanstd_score_charlson=mean(charl_score)/max(max_charl_score), meanstd_ghm_severity=mean(gpcompx, na.rm=TRUE)/max(max_gpcompx), meanstd_score_elixhauser=mean(elix_score)/max(max_elix_score)) %>% ungroup()
      plot_ly(data=temp1, x = ~temps, y = ~meanstd_score_charlson, name = 'Mean standardised Charlson score', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
        add_trace(y = ~meanstd_ghm_severity, name = 'Mean standardised GHM severity', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash')) %>%
        add_trace(y = ~meanstd_score_elixhauser, name = 'Mean standardised Elixhauser score', line = list(color = 'rgb(0, 0, 0)', width = 4)) %>%
        layout(title = "Evolution de la comorbidité des séjours", xaxis = list(title = "Temps"), yaxis = list (title = "Comorbidité standardisée sur la période"))
      } else {
        plot_ly() %>% layout(title='pas de score calculé')
      }
    })

    output$activ10 <- renderPlotly({
      
      if(score==1) {
      temp1 <- rsa %>% dplyr::filter(as.numeric(ansor) >= anno-1, as.numeric(moissor) <= mese) %>% dplyr::group_by(annee=factor(ansor), score=factor(charl_index), .drop=FALSE) %>%  dplyr::summarise(activ=n())
      plot_ly(type = 'bar', orientation = 'h', data=temp1, x=~activ, y=~score, color=~annee) %>% layout(title='Evolution de la comorbidité', xaxis = list(title = 'Nombre de séjours'), yaxis = list(title = 'Score de Charlson'), barmode = 'group')
      # temp1 <- rsa %>% dplyr::filter(as.numeric(ansor) >= anno-1, as.numeric(moissor) <= mese) %>% dplyr::group_by(annee=factor(ansor), score=factor(elix_index), .drop=FALSE) %>%  dplyr::summarise(activ=n())
      # plot_ly(type = 'bar', orientation = 'h', data=temp1, x=~activ, y=~score, color=~annee) %>% layout(title='Evolution de la comorbidité', xaxis = list(title = 'Nombre de séjours'), yaxis = list(title = 'Score de Elixhauser'), barmode = 'group')
      } else {
        plot_ly() %>% layout(title='pas de score calculé')
      }
      
    })
    
    
    output$activ11 <- DT::renderDT( {
      
      rsa %>% dplyr::filter(as.numeric(moissor)<=mese, as.numeric(ansor)>=anno-1) %>% dplyr::group_by(Année=ansor, Racine = libelle_racine, "Domaine d'activité"=libelle_da, "Groupe d'activité"=libelle_ga, GHM=ghm) %>% dplyr::summarise("Nbr de séjours"=n_distinct(nas), DMS=round(sum(duree)/n_distinct(nas), 1), "Pourcentage de monorums"=round(sum(nbrum[nbrum==1])/sum(nbrum)*100, 1))
      
    }, caption=paste0("Détail des GHM passés par ", unitemed," par année jusqu'à M", mese), rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
    
    
  } )
  
  
  observeEvent(input$menuchoice, {
    
    if(input$menuchoice=="valoglob") {
      
      # output$valo0 <- renderPlotly({
      #   temp1 <- giacimento %>% dplyr::group_by(severite=substr(ghm, 6, 6)) %>% dplyr::summarise(diff=sum(diff), recghm=sum(recghm), theoriq=sum(theoriq), activ=sum(nghm), activante=sum(nghm_giac, na.rm=TRUE))
      #   p1 <- plot_ly(data=temp1, x = ~recghm, y = ~severite, name = 'recettes réelles', type = 'bar', orientation = 'h', color = 'rgb(205, 12, 24)') %>%
      #     add_trace(x = ~theoriq, name = 'recettes théoriques (% sev./rac. année préc.)', color = 'rgb(22, 96, 167)') 
      #   p2 <- plot_ly(type = 'scatter', mode = 'lines+markers', data=temp1, x=~activ, y=~severite, name="activité année en cours", color = 'rgb(205, 12, 24)') %>%
      #     add_trace(x = ~activante, name = 'activité année précédente', color = 'rgb(22, 96, 167)')  
      #   subplot(p1, p2) %>% layout(title='Répartition des recettes (€) et nombre de séjours')
      #   
      # })
      
      output$valo0 <- DT::renderDT( {
        
        rsa %>% dplyr::filter(as.numeric(moissor)<=mese, as.numeric(ansor)>=anno-1)  %>% dplyr::summarise("Nbr de séjours annee n"=n_distinct(nas[ansor==anno]), "Séjours monorums n"=n_distinct(nas[nbrum==1 & ansor==anno]), "Valorisation GHS total annee n"=sum(rec_totale[ansor==anno]), "Valorisation monorum n"=sum(rec_totale[nbrum==1 & ansor==anno]), "Nbr de séjours annee n-1"=n_distinct(nas[ansor==anno-1]), "Séjours monorums n-1"=n_distinct(nas[nbrum==1 & ansor==anno-1]), "Valorisation GHS total annee n-1"=sum(rec_totale[ansor==anno-1]), "Valorisation monorum n-1"=sum(rec_totale[nbrum==1 & ansor==anno-1]), "IP moyen n"=mean(ip[ansor==anno], na.rm=TRUE), "IP moyen n-1"=mean(ip[ansor==anno-1], na.rm=TRUE)) %>% dplyr::mutate_at(vars(2:8), ~round(., 0)) %>% dplyr::mutate_at(vars9:(10), ~round(., 3))
        
      }, caption="Valorisation globale des séjours passés par l'unité, par an", rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
      
      
      output$valo1 <- DT::renderDT( {
        
        rsa %>% dplyr::filter(as.numeric(moissor)<=mese, as.numeric(ansor)>=anno-1) %>% dplyr::group_by("Mois de sortie"=moissor) %>% dplyr::summarise("Nbr de séjours annee n"=n_distinct(nas[ansor==anno]), "Séjours monorums n"=n_distinct(nas[nbrum==1 & ansor==anno]), "Valorisation GHS total annee n"=sum(rec_totale[ansor==anno]), "Valorisation monorum n"=sum(rec_totale[nbrum==1 & ansor==anno]), "Nbr de séjours annee n-1"=n_distinct(nas[ansor==anno-1]), "Séjours monorums n-1"=n_distinct(nas[nbrum==1 & ansor==anno-1]), "Valorisation GHS total annee n-1"=sum(rec_totale[ansor==anno-1]), "Valorisation monorum n-1"=sum(rec_totale[nbrum==1 & ansor==anno-1]), "IP moyen n"=mean(ip[ansor==anno], na.rm=TRUE), "IP moyen n-1"=mean(ip[ansor==anno-1], na.rm=TRUE)) %>% dplyr::mutate_at(vars(2:9), ~round(., 0)) %>% dplyr::mutate_at(vars(10:11), ~round(., 3))
        
      }, caption="Valorisation globale des séjours passés par l'unité, par mois", rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
      
      output$valo2 <- renderPlot({
        
        naselect <- rum %>% dplyr::filter(uma_locale2 %in% unita) %>% dplyr::select(nas)  
        
        valograph2 <- rsa %>% dplyr::filter(as.numeric(moissor)<=mese, as.numeric(ansor)>=anno-1) %>% dplyr::select(nas, ghm, duree, ansor, rec_totale)
        
        valograph2$ansor <- valograph2$ansor %>% as.factor %>% relevel(ref = as.character(anno))
        
        ggplot(valograph2, aes(x = ansor, y = rec_totale, fill = ansor)) +
          geom_violin() +
          stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, aes(color = "moyenne des valorisations")) +    # douille
          scale_color_manual(name = "", values = "darkblue") +   # suite de la douille
          labs(x = "Année", y = "Prix de séjour (€)") +
          guides(fill = FALSE) +
          theme_gray()
        
      })
      
      output$valo3 <- renderPlotly({
        
        valograph3 <- rsa %>% dplyr::filter(as.numeric(moissor)<=mese, as.numeric(ansor)>=anno-1) %>% dplyr::select(nas, ghm, duree, ansor, rec_totale)
        
        plot_ly(valograph3, x = ~duree, y = ~rec_totale, color = ~ansor, type = 'scatter', mode = 'markers', hoverinfo = 'text', text = ~paste('</br> nas: ', nas, '</br> GHM: ', ghm, '</br> duree sejour: ', duree, '</br> recette tot: ', round(rec_totale,1))) %>% layout(titlefont=list(size=18), xaxis=list(autorange = TRUE,title = "Durée des séjours"), yaxis=list(autorange = TRUE,title = "Valorisation des séjours (€)"))
        
      })
      
      output$valo4 <- renderPlotly({
        
        temp1 <- rsa %>% dplyr::mutate(ghs=noghs) %>% dplyr::filter(ansor>=anno-1) %>% dplyr::mutate(type=factor(substr(ghm,3,3)), temps=as.factor(paste0(ansor, " - M", moissor))) %>% dplyr::group_by(type, temps) %>% dplyr::summarise(recettes=sum(rec_totale)) %>% dplyr::ungroup()
        ggplotly(ggplot(temp1, (aes(x = temps,  y = recettes, group = type, fill = type))) + geom_area() + theme(axis.text.x = element_text(angle = 66)), tooltip = "y" )
        # ggplotly(ggplot(data=temp1 , aes(group=type)) + geom_area(aes(x=temps, y=recettes, fill=type)) + theme(axis.text.x = element_text(angle = 66)) )
        
      })
      
      output$valo5 <- renderPlotly({
        
        temp1 <- rsa %>% dplyr::mutate(ghs=noghs) %>% dplyr::filter(ansor>=anno-1) %>% dplyr::mutate(severite=factor(substr(ghm,6,6)), temps=as.factor(paste0(ansor, " - M", moissor))) %>% dplyr::group_by(severite, temps) %>% dplyr::summarise(recettes=sum(rec_totale)) %>% dplyr::ungroup()
        ggplotly(ggplot(temp1, (aes(x = temps,  y = recettes, group = severite, fill = severite))) + geom_area() + theme(axis.text.x = element_text(angle = 66)), tooltip = "y" )
        # ggplotly(ggplot(data=temp1, aes(group=severite)) + geom_area(aes(x=temps, y=recettes, fill=severite)) + theme(axis.text.x = element_text(angle = 66)) )
        
      })
      
      output$valo6 <- renderPlotly({
        
        temp1 <- left_join(rsa %>% dplyr::mutate(ghs=noghs), tarifs %>% dplyr::select(ghs, ghm, borne_basse_t=borne_basse, borne_haute_t=borne_haute, anseqta)) %>% dplyr::filter(ansor>=anno-1, duree>borne_haute_t)
        
        plot_ly(data = temp1, x = ~paste0(ansor, " - M", moissor), y = ~duree, type = 'scatter', mode = 'markers', marker = list(size = ~rec_totale/1000, opacity = 0.5), text=paste("recette=",round(temp1$rec_totale,0), " €")) %>% layout(title="Durée et valorisation des séjours dépassant la borne haute", xaxis = list(title = "Temps"), yaxis = list(title = "Durée totale du séjour (jours)"))
        
      })
      
      output$valo7 <- renderPlotly({
          
        temp1 <- rsa %>% dplyr::filter(ansor>=anno-1) %>% dplyr::mutate(temps=paste0(ansor, " - M", moissor)) %>% dplyr::group_by(temps) %>% dplyr::summarise(supplements_SI=sum(nbsupsi), supplements_REA=sum(nbsuprea), supplements_STF=sum(nbsupstf), supplements_SRC=sum(nbsupsrc)) %>% ungroup()
        
        plot_ly(data=temp1, x = ~temps, y = ~supplements_REA, name = 'REA', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
          add_trace(y = ~supplements_SI, name = 'SI', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
          add_trace(y = ~supplements_STF, name = 'STF', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) %>%
          add_trace(y = ~supplements_SRC, name = 'SRC', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
          layout(title = "Evolution mensuelle du nombre de supplements", xaxis = list(title = "Temps"), yaxis = list (title = "Nombre de suppléments"))
        
        })

    } 
  })
  
  observeEvent(input$menuchoice, {
    
    if(input$menuchoice=="pmct") {
      
      twosteps <- function (x, var, niv1='ansor_sej', niv2='moissor_sej', niv3='nas', niv4='LIBUM', fct=list(mean=mean, median=median, rec=sum, nbrap=n_distinct)) {
        dplyr::group_by(x, !!sym(niv1), !!sym(niv2), !!sym(niv3), !!sym(niv4)) %>% dplyr::summarise(s=sum(!!sym(var), na.rm=TRUE)) %>% dplyr::group_by(!!sym(niv4), !!sym(niv1), !!sym(niv2)) %>% dplyr::summarise_at('s', fct, na.rm = TRUE) %>% dplyr::rename_at(vars(names(.) %>% tail(length(fct))), ~ paste0(var,"_",.))
      }
      
      output$pmct1 <- renderPlot({
        
        df1 <- rum %>% dplyr::filter(LIBUM %in% unitemed, ansor_sej>=anno-2) %>% dplyr::group_by(LIBUM, ansor_sej, moissor_sej) %>% dplyr::summarise(pmct_pmctmonotime1=sum(valopmctmonotime1)/n_distinct(nas))  
        df2 <- rum %>% dplyr::filter(LIBUM %in% unitemed, ansor_sej>=anno-2) %>% dplyr::group_by(LIBUM, ansor_sej) %>% dplyr::summarise(pmct_pmctmonotime1=sum(valopmctmonotime1)/n_distinct(nas))
        df3 <- rum %>% dplyr::filter(LIBUM %in% unitemed, ansor_sej==anno, as.numeric(moissor_sej)<=mese) %>% dplyr::group_by(LIBUM, ansor_sej) %>% dplyr::summarise(valopmctmonotime1_tarifsante_mean=sum(valopmctmonotime1_tarifsante)/n_distinct(nas))
        df4 <- rum %>% dplyr::filter(LIBUM %in% unitemed, ansor_sej==anonconsol, as.numeric(moissor_sej)==mnonconsol) %>% dplyr::group_by(LIBUM, ansor_sej) %>% dplyr::summarise(valopmctmonotime1_nonconsol_mean=sum(valopmctmonotime1_nonconsol)/n_distinct(nas))
        
        ggplot(df1) +            # PMCT consolidé (année n et n-1)
          geom_point(aes(x = moissor_sej, y = pmct_pmctmonotime1, color = as.factor(ansor_sej), shape = as.factor(ansor_sej))) + 
          geom_line(aes(x = moissor_sej, y = pmct_pmctmonotime1, group = ansor_sej, color =  as.factor(ansor_sej))) + 
          geom_hline(data=df2, aes(yintercept = pmct_pmctmonotime1, group = ansor_sej, color =  as.factor(ansor_sej)), linetype = 2) +
          geom_point(data=df3, aes(x = mese, y = valopmctmonotime1_tarifsante_mean, color = as.factor(ansor_sej), shape = as.factor(ansor_sej))) + 
          geom_point(data=df4, aes(x = mnonconsol, y = valopmctmonotime1_nonconsol_mean, color = as.factor(ansor_sej), shape = as.factor(ansor_sej))) + 
          geom_text(data=df3, aes(x = mese, y = valopmctmonotime1_tarifsante_mean, label=paste0("M", mese, " de ", anno, " - tarifs antérieurs")), hjust=0, vjust=0) +
          geom_text(data=df4, aes(x = mnonconsol, y = valopmctmonotime1_nonconsol_mean, label=paste0("M", mnonconsol, " de ", anonconsol, " - non consolidé")), hjust=0, vjust=0) +
          labs(x = "Mois de l'année", y = "PMCT (€)") + 
          scale_color_discrete(name = "Année") + scale_shape_discrete(name = "Année") +
          theme_gray()
        
      })
      
      output$pmct2 <- DT::renderDT({
        
        tempmct1 <- twosteps(x=rum %>% dplyr::filter(LIBUM %in% unitemed, as.numeric(moissor_sej)<=mese), var='valopmctmonotime1', niv2='')
        tempmct2 <- twosteps(x=rum %>% dplyr::filter(LIBUM %in% unitemed, as.numeric(moissor_sej)<=mese), var='valopmctmonotime1_tarifsante', niv2='')
        tempmct3 <- twosteps(x=rum %>% dplyr::filter(LIBUM %in% unitemed, ansor_sej==anonconsol, as.numeric(moissor_sej)==mnonconsol), var='valopmctmonotime1_nonconsol', niv2='')
        dplyr::left_join(tempmct1, tempmct2) %>% dplyr::left_join(., tempmct3) %>% dplyr::ungroup() %>% dplyr::select(ansor_sej, dplyr::ends_with("_mean"), dplyr::ends_with("_median")) %>% dplyr::rename("Année de sortie"=ansor_sej, PMCT=valopmctmonotime1_mean, "PMCT tarifs antérieurs"=valopmctmonotime1_tarifsante_mean, !!paste0("PMCT non consolidé à M", mnonconsol):=valopmctmonotime1_nonconsol_mean, PMedianCT=valopmctmonotime1_median, "PMedianCT tarifs antérieurs"=valopmctmonotime1_tarifsante_median, !!paste0("PMedianCT non consolidé à M", mnonconsol):=valopmctmonotime1_nonconsol_median) %>% dplyr::mutate_at(vars(2:7), ~round(., 1))
        
      }, caption=paste0("PMCT annuel à M", mese, " pour ", unitemed, " selon les tarifs en cours ou précédent avec PMCT consolidé à M", mnonconsol), rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
      
      output$pmct3 <- DT::renderDT({
        
        rum %>% dplyr::filter(LIBUM %in% unitemed, ansor_sej >= anno-1) %>% dplyr::group_by(nas, type=substr(cdghm,3,3), ansor_sej) %>% dplyr::summarise(s1=sum(valopmctmonotime1, na.rm = TRUE), s2=sum(valopmctmonotime2, na.rm = TRUE)) %>% dplyr::group_by(type, "Année"=ansor_sej) %>% dplyr::summarise("Valorisation totale ventilée selon pmct monorums et journées" = sum(s1, na.rm = TRUE), "PMCT selon pmct monorums et journées" = mean(s1, na.rm = TRUE), "mediane" =  median(s1, na.rm = TRUE), "Valorisation totale ventilée selon pmct monorums et journées à parité" = sum(s2, na.rm = TRUE), "PMCT selon pmct monorums et journées à parité" = mean(s2, na.rm = TRUE), "mediane" = median(s2, na.rm = TRUE)) %>% dplyr::mutate_at(vars(3:7), ~round(., 1))
        
      }, caption=paste0(anno-1, " - ",anno, ": PMCT en CHKMZ à M", mese), rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
      
      output$pmct4 <- DT::renderDT({
        
        rum %>% dplyr::filter(LIBUM %in% unitemed, ansor_sej >= anno-1) %>% dplyr::group_by(nas, sévérité=(substr(cdghm,6,6)), ansor_sej) %>% dplyr::summarise(s1=sum(valopmctmonotime1, na.rm = TRUE), s2=sum(valopmctmonotime2, na.rm = TRUE)) %>% dplyr::group_by(sévérité, "Année"=ansor_sej) %>% dplyr::summarise("Valorisation totale ventilée selon pmct monorums et journées" = sum(s1, na.rm = TRUE), "PMCT selon pmct monorums et journées" = mean(s1, na.rm = TRUE), "mediane" =  median(s1, na.rm = TRUE), "Valorisation totale ventilée selon pmct monorums et journées à parité" = sum(s2, na.rm = TRUE), "PMCT selon pmct monorums et journées à parité" = mean(s2, na.rm = TRUE), "mediane" = median(s2, na.rm = TRUE))  %>% dplyr::mutate_at(vars(3:7), ~round(., 1))
        
      }, caption=paste0(anno-1, " - ",anno, ": PMCT selon sévérité à M", mese), rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
      
      output$pmct5 <- renderPlot({
        
        pmctgraph5 <- rum %>% dplyr::filter(LIBUM %in% unitemed, as.numeric(ansor_sej) >= anno-1, as.numeric(moissor_sej) <= mese) %>% dplyr::select(norss, nas, cdghm, dureesejpart, duree_sej, LIBUM, ansor_sej, valotime:valopmctmonotime2)
        
        pmctgraph5$ansor_sej <- pmctgraph5$ansor_sej %>% as.factor %>% relevel(ref = as.character(anno))
        
        ggplot(pmctgraph5, aes(x = ansor_sej, y = valopmctmonotime1, fill = ansor_sej)) + 
          geom_violin() + 
          stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, aes(color = "moyenne des valorisations")) +    # douille
          scale_color_manual(name = "", values = "darkblue") +   # suite de la douille
          labs(x = "Année", y = "Prix de séjour (€)") +
          guides(fill = FALSE) + 
          theme_gray()
        
      })
      
      output$pmct6 <- renderPlotly({
        
        pmctgraph6 <- rum %>% dplyr::filter(LIBUM %in% unitemed, as.numeric(ansor_sej) >= anno-1, as.numeric(moissor_sej) <= mese) %>% dplyr::select(norss, nas, cdghm, dureesejpart, duree_sej, LIBUM, ansor_sej, valotime:valopmctmonotime2)
        
        plot_ly(pmctgraph6, x = ~dureesejpart, y = ~valopmctmonotime1, color = ~ansor_sej, type = 'scatter', mode = 'markers', hoverinfo = 'text', text = ~paste('</br> norss: ', norss, '</br> nas: ', nas, '</br> GHM: ', cdghm, '</br> durees: ', dureesejpart, " / ", duree_sej, '</br> valo: ', round(valopmctmonotime1,1))) %>% layout(titlefont=list(size=18), xaxis=list(autorange = TRUE,title = "Durée des rums"), yaxis=list(autorange = TRUE, title = "Valorisation des rums (€)"))
        
      })
      
      output$pmct7 <- DT::renderDT({
        rum %>% dplyr::filter(LIBUM %in% unitemed, as.numeric(ansor_sej)>=anno-1, as.numeric(moissor_sej) <= mese) %>% dplyr::group_by("Année"=ansor_sej) %>% dplyr::summarise("Nbr de séjours"=n_distinct(nas), "Nbr de séjours monorums"=n_distinct(nas[nbrum_sej==1]), "Pourcentage de séjours monorums"=round(n_distinct(nas[nbrum_sej==1])/n_distinct(nas)*100,2), "Pourcentage de séjours multirums"=round(n_distinct(nas[nbrum_sej>1])/n_distinct(nas)*100,2))
      }, caption=paste("Proportion des sejours monorums pour", unitemed, " à M", mese), rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
      
      output$pmct8 <- DT::renderDT({
        pmctmono %>% dplyr::mutate(ansor_sej=ansor) %>% left_join(., rum %>% dplyr::select(cdurm, ansor_sej, LIBUM) %>% distinct()) %>% dplyr::filter(LIBUM %in% unitemed, as.numeric(ansor_sej)>=anno-1) %>% dplyr::group_by(LIBUM, ansor_sej) %>% dplyr::summarise(recmono=sum(recbeenosup), nbmono=sum(nbnosup)) %>% dplyr::transmute("Année (12 derniers mois)"=ansor_sej, "Nbr de séjours monorums sur 12 mois"=nbmono, "PMCT monorum sur 12 mois"=round(recmono/nbmono,1)) %>% dplyr::ungroup() %>% dplyr::select(-LIBUM)
      }, caption=paste("PMCT des sejours monorums pour", unitemed, "sur les 12 derniers mois (utilisé pour les clefs de répartition)"), rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
      
      output$pmct9 <- DT::renderDT({
        listeum <- rum %>% filter(ansor_sej==anno, as.numeric(moissor_sej)<=mese) %>% filter(norss%in%norss[LIBUM %in% unitemed]) %>% dplyr::count(LIBUM) %>% arrange(-n) %>% dplyr::select(LIBUM) %>% unique() 
        pmctmono %>% dplyr::mutate(ansor_sej=ansor) %>% left_join(., rum %>% dplyr::select(cdurm, ansor_sej, LIBUM) %>% distinct()) %>% dplyr::filter(LIBUM %in% listeum$LIBUM, !LIBUM %in% unitemed, as.numeric(ansor)>=anno-1) %>% dplyr::mutate(ansor_sej=as.integer(ansor_sej)) %>%  dplyr::group_by(LIBUM, ansor_sej) %>% dplyr::summarise(recmono=sum(recbeenosup), nbmono=sum(nbnosup)) %>% dplyr::transmute("Année (12 derniers mois)"=ansor_sej, "Nbr de séjours monorums"=nbmono, "PMCT monorum"=round(recmono/nbmono,1)) %>% dplyr::ungroup() %>% dplyr::rename(unite=LIBUM)
      }, caption=paste("PMCT des seuls sejours monorums pour les unités partageant des sejours avec", unitemed, "sur les 12 derniers mois (utilisé pour les clefs de répartition)"), rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
      
      rad <- rum %>% dplyr::filter(nbrum_sej >= 2, is.element(nas, nas[LIBUM %in% unitemed]), as.numeric(ansor_sej) >= anno-1, as.numeric(moissor_sej) <= mese) %>% dplyr::select(nas, LIBUM, ansor_sej, dureesejpart, duree_sej, norum, nbrum_sej, valotime, valopmctmono, valopmctmonotime1, valopmctmonotime2) %>% dplyr::group_by(annee=factor(ansor_sej), unite=factor(LIBUM), .drop=FALSE) %>%  dplyr::summarise(prix_temps = sum(valotime, na.rm=TRUE), prix_pmct = sum(valopmctmono, na.rm=TRUE), prix_temps_pmct1 = sum(valopmctmonotime1, na.rm=TRUE), prix_temps_pmct2 = sum(valopmctmonotime2, na.rm=TRUE)) %>% dplyr::mutate(ratio_temps = 100 * prix_temps / sum(prix_temps), ratio_pmct = 100 * prix_pmct / sum(prix_pmct), ratio_temps_pmct1 = 100 * prix_temps_pmct1 / sum(prix_temps_pmct1), ratio_temps_pmct2 = 100 * prix_temps_pmct2 / sum(prix_temps_pmct2)) %>% dplyr::ungroup() %>% dplyr::filter(is.element(unite, unite[ratio_temps_pmct1>2]) )
      
      output$pmct10 <- renderPlotly({
        plot_ly(type = 'scatterpolar', fill = 'toself') %>%
          add_trace( r = rad$ratio_temps_pmct1[rad$annee==anno],
                     theta = as.character(rad$unite[rad$annee==anno]),
                     name = anno ) %>%
          add_trace( r = rad$ratio_temps_pmct1[rad$annee==anno-1],
                     theta = as.character(rad$unite[rad$annee==anno-1]),
                     name = anno-1 ) %>%
          layout( polar = list(radialaxis = list(visible = TRUE, range = c(0,max(rad$ratio_temps_pmct1)) ) ) )
      })
      
      output$pmct11 <- renderPlot({
        ball <- left_join(rad, left_join(pmctmono, rum %>% dplyr::mutate(ansor=as.character(ansor_sej)) %>% dplyr::select(cdurm, LIBUM, ansor) %>% distinct) %>% dplyr::group_by(LIBUM, ansor) %>% dplyr::summarise(r=sum(recbeenosup), n=sum(nbnosup)) %>% dplyr::mutate(pmctmono=r/n) %>% dplyr::select(LIBUM, ansor, pmctmono) %>% dplyr::rename(annee=ansor, unite=LIBUM) ) %>% dplyr::rename(pmct_mono_nosup=pmctmono)
        ggplot(ball) + 
          geom_point(aes(x = ratio_temps, y = pmct_mono_nosup, size = ratio_temps_pmct1, color = unite)) +
          scale_size("% du prix", range = c(2, 15)) +
          scale_color_brewer("URM", palette = "Set3") +
          labs(x = "proportion du temps passé dans chaque service (%)", y = "PMCT monorum (sans suppléments)") +
          guides(size = FALSE) + 
          theme_gray() + 
          theme(legend.position = "bottom", legend.text = element_text(size = 6), legend.title = element_text(size = 10, face = "bold")) +
          facet_wrap(~ annee)
      })
    }
  })
  
  observeEvent(input$menuchoice, {
    
    if(input$menuchoice=="parcours") {
      
      rad <- rum %>% dplyr::filter(nbrum_sej >= 2, is.element(nas, nas[LIBUM %in% unitemed]), as.numeric(ansor_sej) >= anno-1, as.numeric(moissor_sej) <= mese) %>% dplyr::select(nas, LIBUM, ansor_sej, dureesejpart, duree_sej, norum, nbrum_sej, valotime, valopmctmono, valopmctmonotime1, valopmctmonotime2) %>% dplyr::group_by(annee=factor(ansor_sej), unite=factor(LIBUM), .drop=FALSE) %>%  dplyr::summarise(prix_temps = sum(valotime, na.rm=TRUE), prix_pmct = sum(valopmctmono, na.rm=TRUE), prix_temps_pmct1 = sum(valopmctmonotime1, na.rm=TRUE), prix_temps_pmct2 = sum(valopmctmonotime2, na.rm=TRUE)) %>% dplyr::mutate(ratio_temps = 100 * prix_temps / sum(prix_temps), ratio_pmct = 100 * prix_pmct / sum(prix_pmct), ratio_temps_pmct1 = 100 * prix_temps_pmct1 / sum(prix_temps_pmct1), ratio_temps_pmct2 = 100 * prix_temps_pmct2 / sum(prix_temps_pmct2)) %>% dplyr::ungroup() %>% dplyr::filter(is.element(unite, unite[ratio_temps_pmct1>2]) )
      ball <- left_join(rad, left_join(pmctmono, rum %>% dplyr::mutate(ansor=as.character(ansor_sej)) %>% dplyr::select(cdurm, LIBUM, ansor) %>% distinct) %>% dplyr::group_by(LIBUM, ansor) %>% dplyr::summarise(r=sum(recbeenosup), n=sum(nbnosup)) %>% dplyr::mutate(pmctmono=r/n) %>% dplyr::select(LIBUM, ansor, pmctmono) %>% dplyr::rename(annee=ansor, unite=LIBUM) ) %>% dplyr::rename(pmct_mono_nosup=pmctmono)
      libum <- ball %>% dplyr::arrange(unite) %>% dplyr::select(unite) %>% unique()
      colors <- brewer.pal(length(libum), "Set3")
      
      pal <- function (n, c = 100, l = 72, start = -4, end = 360, fixup = TRUE, gamma = NULL, alpha = 1, ...) {
        if (!is.null(gamma)) warning("'gamma' is deprecated and has no effect")
        if (n < 1L) return(character(0L))
        rval <- hex(polarLUV(L = l, C = c, H = seq(start, end, length = n)), fixup = fixup, ...)
        if (!missing(alpha)) {
          alpha <- pmax(pmin(alpha, 1), 0)
          alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)),
                          width = 2L, upper.case = TRUE)
          rval <- paste(rval, alpha, sep = "")
        }
        return(rval)
      }
      
      custom.message = "function (d) {root = d; while (root.parent) {root = root.parent} p = (100*d.value/root.value).toPrecision(3); msg = p+' %<br/>'+d.value+' of '+root.value;  return msg;}"
      
      output$parcours1 <- renderSunburst({ 
        daz <- rum %>% dplyr::select(norss, nas, norum, nbrum_sej, LIBUM, ansor_sej, moissor_sej, uma_locale2, libelle_um) %>%
          dplyr::filter(nbrum_sej >= 2, 
                        norss%in%norss[LIBUM %in% unitemed], 
                        as.numeric(ansor_sej) == anno, 
                        as.numeric(moissor_sej) <= mese)
        lib <- c(libum, unique(daz$LIBUM[!daz$LIBUM %in% libum]))
        colrs <- c(colors, pal(length(lib) - length(libum)))
        daz %>% dplyr::group_by(nas) %>% dplyr::summarise(URMS=paste0(LIBUM,collapse=",")) -> temp2
        temp2 %>% dplyr::filter(grepl(",",URMS)) %>% dplyr::group_by(URMS) %>% dplyr::summarise(nbrSej=n_distinct(nas)) %>% dplyr::mutate(URMS=gsub("-"," ",URMS),URMS=gsub(",","-",URMS)) -> temp3
        sunburst(temp3, legend = list(w = 300, h =15, r =10, s = 2), explanation = custom.message, colors = list(range = colrs, domain = gsub("-", " ", lib)))
      })
      
      output$parcours2 <- renderDT({
        
        rum %>% dplyr::select(norss, nas, norum, nbrum_sej, LIBUM, ansor_sej, moissor_sej, uma_locale2, libelle_um, cdghm, libelle_racine_sej, libelle_da_sej, libelle_ga_sej) %>% dplyr::filter(nbrum_sej >= 2, norss%in%norss[LIBUM %in% unitemed], as.numeric(ansor_sej) == anno, as.numeric(moissor_sej) <= mese) %>% dplyr::group_by(nas) %>% dplyr::summarise(URMS=paste0(paste0(uma_locale2," ",libelle_um),collapse=",")) %>% dplyr::left_join(., rum %>% dplyr::select(nas, cdghm, libelle_racine_sej, libelle_da_sej, libelle_ga_sej) %>% dplyr::filter(!duplicated(nas)), by='nas') %>% dplyr::group_by(URMS, libelle_da_sej) %>%  dplyr::summarise(n=n_distinct(nas)) %>% dplyr::arrange(URMS,desc(n)) %>% dplyr::group_by(URMS) %>% dplyr::summarise(GHMS=paste0(strsplit(paste0(paste0(libelle_da_sej,"(",n,")"),collapse=", "), ",")[[1]][1:3], collapse=","), n_urms=sum(n)) %>%  dplyr::arrange(desc(n_urms)) %>% dplyr::filter(row_number()<6)
        
      }, caption="Domaines d'activité des 5 parcours les plus representés", rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
      
      output$parcours3 <- renderSunburst({ 
        # PAS FAIT PAR REGROUPEMENT ICI UNITE MEDICALE UNIQUE, ALORS QUE LE PREMIER SUNBURST EST OK 
        daz <- rum %>% dplyr::select(norss, nas, norum, nbrum_sej, LIBUM, ansor_sej, moissor_sej, uma_locale2, libelle_um) %>%
          dplyr::filter(nbrum_sej >= 2, 
                        norss%in%norss[LIBUM %in% unitemed], 
                        as.numeric(ansor_sej) == anno-1, 
                        as.numeric(moissor_sej) <= mese)  # on peut remplacer 12 par mese si c'est ce qu'on préfère
        lib <- c(libum, unique(daz$LIBUM[!daz$LIBUM %in% libum]))
        colrs <- c(colors, pal(length(lib) - length(libum)))
        daz %>% dplyr::group_by(nas) %>% dplyr::summarise(URMS=paste0(paste0(uma_locale2," ",libelle_um),collapse=",")) -> temp2
        temp2 %>% dplyr::filter(grepl(",",URMS)) %>% dplyr::group_by(URMS) %>% dplyr::summarise(nbrSej=n_distinct(nas)) %>% dplyr::mutate(URMS=gsub("-"," ",URMS),URMS=gsub(",","-",URMS)) -> temp3
        sunburst(temp3, legend = list(w = 300, h = 15, r = 10, s = 2), explanation = custom.message, colors = list(range = colrs, domain = gsub("-", " ", lib)))
      })
      
      output$parcours4 <- renderDT({
        rum %>% dplyr::select(norss, nas, norum, nbrum_sej, LIBUM, ansor_sej, moissor_sej, uma_locale2, libelle_um, cdghm, libelle_racine_sej, libelle_da_sej, libelle_ga_sej) %>% dplyr::filter(nbrum_sej >= 2, norss%in%norss[LIBUM %in% unitemed], as.numeric(ansor_sej) == anno-1, as.numeric(moissor_sej) <= mese) %>% dplyr::group_by(nas) %>% dplyr::summarise(URMS=paste0(paste0(uma_locale2," ",libelle_um),collapse=",")) %>% dplyr::left_join(., rum %>% dplyr::select(nas, cdghm, libelle_racine_sej, libelle_da_sej, libelle_ga_sej) %>% dplyr::filter(!duplicated(nas)), by='nas') %>% dplyr::group_by(URMS, libelle_da_sej) %>%  dplyr::summarise(n=n_distinct(nas)) %>% dplyr::arrange(URMS,desc(n)) %>% dplyr::group_by(URMS) %>% dplyr::summarise(GHMS=paste0(strsplit(paste0(paste0(libelle_da_sej,"(",n,")"),collapse=", "), ",")[[1]][1:3], collapse=","), n_urms=sum(n)) %>%  dplyr::arrange(desc(n_urms)) %>% dplyr::filter(row_number()<6)
      }, caption="Domaines d'activité des 5 parcours les plus representés", rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
      
      output$parcours5 <- renderSunburst({ 
        # PAS FAIT PAR REGROUPEMENT ICI UNITE MEDICALE UNIQUE, ALORS QUE LE PREMIER SUNBURST EST OK 
        daz <- rum %>% dplyr::select(norss, nas, norum, nbrum_sej, LIBUM, ansor_sej, moissor_sej, uma_locale2, libelle_um) %>%
          dplyr::filter(nbrum_sej >= 2, 
                        norss%in%norss[LIBUM %in% unitemed], 
                        as.numeric(ansor_sej) == anno-1, 
                        as.numeric(moissor_sej) <= 12)  # on peut remplacer 12 par mese si c'est ce qu'on préfère
        lib <- c(libum, unique(daz$LIBUM[!daz$LIBUM %in% libum]))
        colrs <- c(colors, pal(length(lib) - length(libum)))
        daz %>% dplyr::group_by(nas) %>% dplyr::summarise(URMS=paste0(paste0(uma_locale2,"",libelle_um),collapse=",")) -> temp2
        temp2 %>% dplyr::filter(grepl(",",URMS)) %>% dplyr::group_by(URMS) %>% dplyr::summarise(nbrSej=n_distinct(nas)) %>% dplyr::mutate(URMS=gsub("-"," ",URMS),URMS=gsub(",","-",URMS)) -> temp3
        sunburst(temp3, legend = list(w = 300, h = 15, r = 10, s = 2), explanation = custom.message, colors = list(range = colrs, domain = gsub("-", " ", lib)))
      })
      
      output$parcours6 <- renderDT({
        rum %>% dplyr::select(norss, nas, norum, nbrum_sej, LIBUM, ansor_sej, moissor_sej, uma_locale2, libelle_um, cdghm, libelle_racine_sej, libelle_da_sej, libelle_ga_sej) %>% dplyr::filter(nbrum_sej >= 2, norss%in%norss[LIBUM %in% unitemed], as.numeric(ansor_sej) == anno-1, as.numeric(moissor_sej) <= 12) %>% dplyr::group_by(nas) %>% dplyr::summarise(URMS=paste0(paste0(uma_locale2," ",libelle_um),collapse=",")) %>% dplyr::left_join(., rum %>% dplyr::select(nas, cdghm, libelle_racine_sej, libelle_da_sej, libelle_ga_sej) %>% dplyr::filter(!duplicated(nas)), by='nas') %>% dplyr::group_by(URMS, libelle_da_sej) %>%  dplyr::summarise(n=n_distinct(nas)) %>% dplyr::arrange(URMS,desc(n)) %>% dplyr::group_by(URMS) %>% dplyr::summarise(GHMS=paste0(strsplit(paste0(paste0(libelle_da_sej,"(",n,")"),collapse=", "), ",")[[1]][1:3], collapse=","), n_urms=sum(n)) %>%  dplyr::arrange(desc(n_urms)) %>% dplyr::filter(row_number()<6)
      }, caption="Domaines d'activité des 5 parcours les plus representés", rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
      
      output$parcours7 <- renderForceNetwork({
        rum %>% dplyr::filter(ansor_sej==anno,as.numeric(moissor_sej)<=mese) -> temp0
        temp0 %>% dplyr::filter(norss%in%norss[LIBUM %in% unitemed]) -> temp1
        temp1 %>% dplyr::group_by(LIBUM) %>% dplyr::summarise(nvalue1=sum(dureesejpart)/10,HOPITAL=first(nofiness),LIBPOLE=first(pole)) %>% dplyr::filter(!duplicated(LIBUM),!is.na(LIBUM)) %>% dplyr::mutate(code=as.integer(row.names(.))-1) -> nodes
        temp1 %>% dplyr::select(norss,norum,LIBUM,nofiness,pole) %>% dplyr::arrange(norss,norum) %>% dplyr::mutate(source=LIBUM,target=ifelse(dplyr::lead(norss)==norss,dplyr::lead(LIBUM),NA)) %>% dplyr::mutate(source=factor(source,levels=nodes$LIBUM,labels=nodes$code), target=factor(target,levels=nodes$LIBUM,labels=nodes$code)) %>% dplyr::filter(is.na(target)==FALSE) %>% dplyr::group_by(seq=paste0(source,"-",target)) %>% dplyr::summarise(source=first(source),target=first(target),lvalue1=n_distinct(norss)) %>% dplyr::select(source,target,lvalue1) -> links
        forceNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", Value = "lvalue1", NodeID = "LIBUM", Group = "LIBPOLE", fontSize = 12, Nodesize = "nvalue1", opacity = 1, arrows=TRUE, zoom=TRUE, charge=-1000, bounded=FALSE, legend=TRUE)
      })
      
      output$parcours8 <- renderForceNetwork({
        rum %>% dplyr::filter(ansor_sej==(anno-1),as.numeric(moissor_sej)<=mese) -> temp0
        temp0 %>% dplyr::filter(norss%in%norss[LIBUM %in% unitemed]) -> temp1
        temp1 %>% dplyr::group_by(LIBUM) %>% dplyr::summarise(nvalue1=sum(dureesejpart)/10,HOPITAL=first(nofiness),LIBPOLE=first(pole)) %>% dplyr::filter(!duplicated(LIBUM),!is.na(LIBUM)) %>% dplyr::mutate(code=as.numeric(row.names(.))-1) -> nodes
        temp1 %>% dplyr::select(norss,norum,LIBUM,nofiness,pole) %>% dplyr::arrange(norss,norum) %>% dplyr::mutate(source=LIBUM,target=ifelse(lead(norss)==norss,lead(LIBUM),NA)) %>% dplyr::mutate(source=factor(source,levels=nodes$LIBUM,labels=nodes$code), target=factor(target,levels=nodes$LIBUM,labels=nodes$code)) %>% dplyr::filter(is.na(target)==FALSE) %>% dplyr::group_by(seq=paste0(source,"-",target)) %>% dplyr::summarise(source=first(source),target=first(target),lvalue1=n_distinct(norss)) %>% dplyr::select(source,target,lvalue1) -> links
        forceNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", Value = "lvalue1", NodeID = "LIBUM", Group = "LIBPOLE", fontSize = 12, Nodesize = "nvalue1", opacity = 1, arrows=TRUE, zoom=TRUE, charge=-1000, bounded=FALSE, legend=TRUE)
      })
      # output$parcours9 <- renderSankeyNetwork({
      #   rum %>% dplyr::filter(ansor_sej==anno,as.numeric(moissor_sej)<=mese) -> temp0
      #   temp0 %>% dplyr::filter(norss%in%norss[LIBUM %in% unitemed]) -> temp1
      #   temp1 %>% dplyr::group_by(LIBUM) %>% dplyr::summarise(nvalue1=sum(dureesejpart)/10,HOPITAL=first(nofiness),LIBPOLE=first(pole)) %>% dplyr::filter(!duplicated(LIBUM),!is.na(LIBUM)) %>% dplyr::mutate(code=as.numeric(row.names(.))-1) -> nodes
      #   temp1 %>% dplyr::select(norss,norum,LIBUM,nofiness,pole) %>% dplyr::arrange(norss,norum) %>% dplyr::mutate(source=LIBUM,target=ifelse(lead(norss)==norss,lead(LIBUM),NA)) %>% dplyr::mutate(source=factor(source,levels=nodes$LIBUM,labels=nodes$code), target=factor(target,levels=nodes$LIBUM,labels=nodes$code)) %>% dplyr::filter(is.na(target)==FALSE) %>% dplyr::group_by(seq=paste0(source,"-",target)) %>% dplyr::summarise(source=first(source),target=first(target),lvalue1=n_distinct(norss)) %>% dplyr::select(source,target,lvalue1) -> links
      #   sankeyNetwork(Links = links, Nodes = nodes[,c('LIBUM')], Source = "source", Target = "target", Value = "lvalue1", NodeID = "LIBUM", fontSize = 12, nodeWidth = 30)   #NBfab: not working, check why.
      # })
      # output$parcours10 <- renderSankeyNetwork({
      #   rum %>% dplyr::filter(ansor_sej==(anno-1),as.numeric(moissor_sej)<=mese) -> temp0
      #   temp0 %>% dplyr::filter(norss%in%norss[LIBUM %in% unitemed]) -> temp1
      #   temp1 %>% dplyr::group_by(LIBUM) %>% dplyr::summarise(nvalue1=sum(dureesejpart)/10,HOPITAL=first(nofiness),LIBPOLE=first(pole)) %>% dplyr::filter(!duplicated(LIBUM),!is.na(LIBUM)) %>% dplyr::mutate(code=as.numeric(row.names(.))-1) -> nodes
      #   temp1 %>% dplyr::select(norss,norum,LIBUM,nofiness,pole) %>% dplyr::arrange(norss,norum) %>% dplyr::mutate(source=LIBUM,target=ifelse(lead(norss)==norss,lead(LIBUM),NA)) %>% dplyr::mutate(source=factor(source,levels=nodes$LIBUM,labels=nodes$code), target=factor(target,levels=nodes$LIBUM,labels=nodes$code)) %>% dplyr::filter(is.na(target)==FALSE) %>% dplyr::group_by(seq=paste0(source,"-",target)) %>% dplyr::summarise(source=first(source),target=first(target),lvalue1=n_distinct(norss)) %>% dplyr::select(source,target,lvalue1) -> links
      #   sankeyNetwork(Links = links, Nodes = nodes[,c('LIBUM')], Source = "source", Target = "target", Value = "lvalue1", NodeID = "LIBUM", fontSize = 12, nodeWidth = 30)   #NBfab: not working, check why.
      # })
      
      output$parcours11 <- renderPlotly({
        # Ci-dessous pour graph à aires mensuel:
        # temp1 <- rum %>% dplyr::filter(ansor_sej>=anno-1, ! LIBUM %in% unitemed) %>% dplyr::mutate(temps=paste0(ansor_sej, " - M", moissor_sej)) %>% dplyr::group_by(autorisation=libelle_typeaut, temps) %>% dplyr::summarise(jours=sum(dureesejpart)) %>% ungroup()
        # plot_ly(data = temp1, x = ~temps, y = ~jours, type = 'scatter', mode='lines', color = ~autorisation) %>% layout(title="Nombre de journées dans les autres services par autorisation", xaxis = list(title = "Temps"), yaxis = list(title = "Journées"), showlegend = FALSE)
        temp1 <- rum %>% dplyr::filter(ansor_sej>=anno-1, as.numeric(moissor_sej)<=mese, ! LIBUM %in% unitemed)  %>% dplyr::group_by(autorisation=libelle_typeaut, Annee=ansor_sej) %>% dplyr::summarise(jours=sum(dureesejpart)) %>% ungroup()
        plot_ly(type = 'bar', orientation = 'h', data=temp1, x=~jours, y=~autorisation, color=~Annee) %>% layout(title=paste0("Nombre de journées dans les autres services selon le type d'autorisation à M", mese), xaxis = list(title = 'Nombre de journées'), yaxis = list(title = "Type d'autorisation", automargin=TRUE, tickangle=-15), barmode = 'group')
      })
      
    } 
  })
  
  observeEvent(input$menuchoice, {
    
    if(input$menuchoice=="anavalo") {
      
      output$anavalo01 <- renderDT({
        rum %>% dplyr::filter(LIBUM %in% unitemed, as.numeric(ansor_sej) == anno | as.numeric(ansor_sej) == anno-1, as.numeric(moissor_sej) <= mese) %>% dplyr::group_by(Année=ansor_sej) %>% dplyr::summarise("Valorisation totale"=sum(round(valopmctmonotime1,1)), "Dont valorisation suppléments"=sum(round(rec_sup_repa,1)), "Valorisation totale tarifs ant."=sum(round(valopmctmonotime1_tarifsante,1)), "Nbr journées rums"=sum(dureesejpart), "Nbr journées séjours"=sum(duree_sej), "Nbr de rums"=length(nofiness))
        }, caption="Valorisation des rums", rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
      
      output$anavalo02 <- renderDT({
        rum %>% dplyr::filter(LIBUM %in% unitemed, as.numeric(ansor_sej) == anno | as.numeric(ansor_sej) == anno-1, as.numeric(moissor_sej) <= mese) %>% dplyr::group_by(Année=ansor_sej, GHM=cdghm) %>% dplyr::summarise("Valorisation totale"=sum(round(valopmctmonotime1,1)), "Dont valorisation suppléments"=sum(round(rec_sup_repa,1)), "Valorisation totale tarifs ant."=sum(round(valopmctmonotime1_tarifsante,1)), "Nbr journées rums"=sum(dureesejpart), "Nbr journées séjours"=sum(duree_sej), "Nbr de rums"=length(nofiness))
      }, caption="Valorisation des rums selon le GHM", rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
      
      output$anavalo03 <- renderDT({
        giac %>% dplyr::select(diff_rec, diff_rec_base, diff_rec_exbh, diff_rec_supp, diff_tarifsante, diff_consol, diff_theoriq, diff_reste, nracreel, nrac_consol) %>% dplyr::summarise_all(sum, na.rm=TRUE) %>% dplyr::mutate_at(vars(1:8), ~round(., 1)) %>% dplyr::rename("Differentiel rec. globale"=diff_rec, "Differentiel rec. base"=diff_rec_base, "Differentiel rec. bornes"=diff_rec_exbh, "Differentiel rec. suppléments"=diff_rec_supp, "Differentiel tarifs"=diff_tarifsante, "Consolidation à n-1"=diff_consol, "Rec. base théorique"=diff_theoriq, "Différentiel restant"=diff_reste, "Nbr de ghs"=nracreel, "Nbr de ghs année précédente"=nrac_consol)
      }, caption="Differentiels de recettes sur 1 an", rownames=FALSE, extensions = 'Buttons', filter = 'top', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')))
      
      temp1 <- giac %>% dplyr::group_by(cmd=substr(racine, 1, 2)) %>% dplyr::summarise(recrac=sum(recrac, na.rm=TRUE), recrac_exbh=sum(recrac_exbh, na.rm=TRUE), recracsup=sum(recracsup, na.rm=TRUE), diff_tarifsante=sum(diff_tarifsante, na.rm=TRUE), theoriq_num=sum(theoriq_num, na.rm=TRUE), recrac_consol=sum(recrac_consol, na.rm=TRUE), recrac_exbh_consol=sum(recrac_exbh_consol, na.rm=TRUE), recracsup_consol=sum(recracsup_consol, na.rm=TRUE), evol_consol=sum(evol_consol, na.rm=TRUE), nrac=sum(nracreel, na.rm=TRUE), nrac_consol=sum(nrac_consol, na.rm=TRUE))
      
      output$anavalo0 <- renderPlotly({
        plotly::plot_ly(data=temp1) %>%
          plotly::add_trace(x = ~recrac, y = ~cmd, name = 'recettes base', type = 'bar', orientation = 'h',  bar = list(color = "blue")) %>%
          plotly::add_trace(x = ~recrac_exbh, y = ~cmd, name = 'recettes bornes', type = 'bar', orientation = 'h',  bar = list(color = "blue")) %>%
          plotly::add_trace(x = ~recracsup, y = ~cmd, name = 'recettes suppléments', type = 'bar', orientation = 'h',  bar = list(color = "blue")) %>%
          plotly::add_trace(x = ~diff_tarifsante, y = ~cmd, name = 'effet tarifs', type = 'bar', orientation = 'h', bar = list(color = "blue")) %>%
          plotly::add_trace(x = ~theoriq_num, y = ~cmd, xaxis="x", name = 'recettes théorique (% sev./rac. année préc.)', type = 'bar', orientation = 'h', bar = list(color = "red")) %>%
          plotly::add_trace(x = ~recrac_consol, y = ~cmd, name = 'recettes base n-1', type = 'bar', orientation = 'h',  bar = list(color = "green")) %>%
          plotly::add_trace(x = ~recrac_exbh_consol, y = ~cmd, name = 'recettes bornes n-1', type = 'bar', orientation = 'h',  bar = list(color = "green")) %>%
          plotly::add_trace(x = ~recracsup_consol, y = ~cmd, name = 'recettes suppléments n-1', type = 'bar', orientation = 'h', bar = list(color = "green")) %>%
          plotly::add_trace(x = ~evol_consol, y = ~cmd, name = 'consolidation totale n-1', type = 'bar', orientation = 'h', bar = list(color = "green")) %>%
          plotly::add_trace(type = 'scatter', mode = 'lines+markers', x=~nrac, y=~cmd, xaxis="x2", name="activité", line = list(color = "orange")) %>%
          plotly::add_trace(type = 'scatter', mode = 'lines+markers', x=~nrac_consol, y=~cmd, xaxis="x2", name="activité n-1", line = list(color = "yellow")) %>%
          plotly::layout(xaxis = list(tickfont = list(color = "blue"),  side = "bottom", anchor = "y", title = "recettes (€)"), xaxis2 = list(tickfont = list(color = "black"), overlaying = "x", side = "top", anchor = "y", title = "activité (nbr de séjours)"))
      })
      
      anavalograph <- rum %>% dplyr::filter(LIBUM %in% unitemed, as.numeric(ansor_sej) == anno | as.numeric(ansor_sej) == anno-1, as.numeric(moissor_sej) <= mese) %>% dplyr::group_by(sum_valo=substr(cdghm,3,3)) %>% dplyr::mutate(année=as.factor(ansor_sej)) %>% dplyr::select(norss, cdghm, dureesejpart, LIBUM, année, valotime:valopmctmonotime2, nbrum_sej, libelle_racine_sej, libelle_da_sej, libelle_ga_sej)
      
      output$anavalo1 <- renderPlotly({
        ggplotly( ggplot(anavalograph, aes(x=substr(cdghm,3,3), y=valopmctmonotime1, fill=année)) + geom_boxplot() + geom_point(aes(x=substr(cdghm,3,3), y=valopmctmonotime1/100), stat = "summary", fun.y=sum, show.legend=TRUE, position=position_dodge(width=0.75), size=4, shape=8) + scale_y_continuous(name = "Valorisation des rums (€)", sec.axis = sec_axis(~./100, name="Somme de la Valorisation / 100 (€)")) + labs(x = "Type de GHM") ) %>% layout(boxmode = "group")
      })
      output$anavalo2 <- renderPlotly({
        ggplotly( ggplot(anavalograph, aes(x=substr(cdghm,6,6), y=valopmctmonotime1, fill=année)) + geom_boxplot() + geom_point(aes(x=substr(cdghm,6,6), y=valopmctmonotime1/100), stat = "summary", fun.y=sum, show.legend=TRUE, position=position_dodge(width=0.75), size=4, shape=8) + scale_y_continuous(name = "Valorisation des rums (€)", sec.axis = sec_axis(~./100, name="Somme de la Valorisation / 100 (€)")) + labs(x = "Type de GHM") ) %>% layout(boxmode = "group")
      })
      output$anavalo3 <- renderPlotly({
        ggplotly( ggplot(anavalograph, aes(x=substr(cdghm,1,2), y=valopmctmonotime1, fill=année)) + geom_boxplot() + geom_point(aes(x=substr(cdghm,1,2), y=valopmctmonotime1/100), stat = "summary", fun.y=sum, show.legend=TRUE, position=position_dodge(width=0.75), size=4, shape=8) + scale_y_continuous(name = "Valorisation des séjours (€)", sec.axis = sec_axis(~./100, name="Somme de la Valorisation / 100 (€)")) + labs(x = "Type de GHM") ) %>% layout(boxmode = "group")
      })
      output$anavalo4 <- renderPlotly({
        ggplotly( ggplot(anavalograph, aes(x=libelle_da_sej, y=valopmctmonotime1, fill=année)) + theme(axis.text.x = element_text(angle = 66)) + geom_boxplot() + geom_point(aes(x=libelle_da_sej, y=valopmctmonotime1/100), stat = "summary", fun.y=sum, show.legend=TRUE, position=position_dodge(width=0.75), size=4, shape=8) + scale_y_continuous(name = "Valorisation des séjours (€)", sec.axis = sec_axis(~./100, name="Somme de la Valorisation / 100 (€)")) + labs(x = "Domaine d'activité") ) %>% layout(boxmode = "group")
      })
      output$anavalo5 <- renderPlotly({
        radact <- rum %>% dplyr::filter(LIBUM %in% unitemed, as.numeric(ansor_sej) >= anno-1, as.numeric(moissor_sej) <= mese) %>% dplyr::select(nas, LIBUM, ansor_sej, dureesejpart, duree_sej, norum, nbrum_sej, cdghm, valopmctmonotime1) %>% dplyr::group_by(annee=factor(ansor_sej), type=factor(substr(cdghm,3,3)), .drop=FALSE) %>%  dplyr::summarise(activ=n(), valo=sum(valopmctmonotime1))
        p1 <- plot_ly(type = 'bar', orientation = 'h', data=radact, x=~activ, y=~type, color=~annee) %>% layout(title='test1', xaxis = list(title = 'Nombre de passages'), yaxis = list(title = 'Type'), barmode = 'group')
        p2 <- plot_ly(type = 'scatter', mode = 'lines+markers', data=radact, x=~valo, y=~type, color=~annee) 
        subplot(p1, p2) %>% layout(title='Activité (rums) et valorisation (€)')
      })
      output$anavalo6 <- renderPlotly({
        radact <- rum %>% dplyr::filter(LIBUM %in% unitemed, as.numeric(ansor_sej) >= anno-1, as.numeric(moissor_sej) <= mese) %>% dplyr::select(nas, LIBUM, ansor_sej, dureesejpart, duree_sej, norum, nbrum_sej, cdghm, valopmctmonotime1) %>% dplyr::group_by(annee=factor(ansor_sej), severite=factor(substr(cdghm,6,6)), .drop=FALSE) %>%  dplyr::summarise(activ=n(), valo=sum(valopmctmonotime1))
        p1 <- plot_ly(type = 'bar', orientation = 'h', data=radact, x=~activ, y=~severite, color=~annee) %>% layout(title='test1', xaxis = list(title = 'Nombre de passages'), yaxis = list(title = 'Severite'), barmode = 'group')
        p2 <- plot_ly(type = 'scatter', mode = 'lines+markers', data=radact, x=~valo, y=~severite, color=~annee) 
        subplot(p1, p2) %>% layout(title='Activité (rums) et valorisation (€)')
      })
      output$anavalo7 <- renderPlotly({
        radact <- rum %>% dplyr::filter(LIBUM %in% unitemed, as.numeric(ansor_sej) >= anno-1, as.numeric(moissor_sej) <= mese) %>% dplyr::select(nas, LIBUM, ansor_sej, dureesejpart, duree_sej, norum, nbrum_sej, cdghm, valopmctmonotime1, libelle_da_sej, libelle_ga_sej) %>% dplyr::group_by(annee=factor(ansor_sej), da=factor(libelle_da_sej), .drop=FALSE) %>%  dplyr::summarise(activ=n(), valo=sum(valopmctmonotime1))
        p1 <- plot_ly(type = 'bar', orientation = 'h', data=radact, x=~activ, y=~da, color=~annee) %>% layout(title='test1', xaxis = list(title = 'Nombre de passages'), yaxis = list(title = 'Dom. activité'), barmode = 'group')
        p2 <- plot_ly(type = 'scatter', mode = 'lines+markers', data=radact, x=~valo, y=~da, color=~annee) 
        subplot(p1, p2, shareY=TRUE) %>% layout(title='Activité (rums) et valorisation (€)')
      })
      output$anavalo8 <- renderPlotly({
        radact <- rum %>% dplyr::filter(LIBUM %in% unitemed, as.numeric(ansor_sej) >= anno-1, as.numeric(moissor_sej) <= mese) %>% dplyr::select(nas, LIBUM, ansor_sej, dureesejpart, duree_sej, norum, nbrum_sej, cdghm, valopmctmonotime1) %>% dplyr::group_by(annee=factor(ansor_sej), cmd=factor(substr(cdghm,1,2)), .drop=FALSE) %>%  dplyr::summarise(activ=n(), valo=sum(valopmctmonotime1))
        p1 <- plot_ly(type = 'bar', orientation = 'h', data=radact, x=~activ, y=~cmd, color=~annee) %>% layout(title='test1', xaxis = list(title = 'Nombre de passages'), yaxis = list(title = 'Severite'), barmode = 'group')
        p2 <- plot_ly(type = 'scatter', mode = 'lines+markers', data=radact, x=~valo, y=~cmd, color=~annee) 
        subplot(p1, p2) %>% layout(title='Activité (rums) et valorisation (€)')
      })
      
      pc <- rum %>% dplyr::filter(LIBUM %in% unitemed, as.numeric(ansor_sej) == anno | as.numeric(ansor_sej) == anno-1, as.numeric(moissor_sej) <= mese) %>% dplyr::mutate(cmd=substr(cdghm,1,2), sortie=ifelse(mdsoue==0, 0, as.integer(mdsoue)-5), entree=ifelse(mdeeue==0, 0, as.integer(mdeeue)-5), severite=ifelse(is.na(as.numeric(substr(cdghm,6,6))),0,substr(cdghm,6,6)), age=as.numeric(d8soue-dtnais)/365.25, typeghm=ifelse(substr(cdghm,3,3)=="C",1,ifelse(substr(cdghm,3,3)=="K", 2, ifelse(substr(cdghm,3,3)=="M", 3, ifelse(substr(cdghm,3,3)=="Z", 4, 5))))) %>%  dplyr::mutate_if(is.character, as.numeric) %>% dplyr::select(dmr=dureesejpart, annee=ansor_sej, nbrum_sej, sexe=sxpmsi, entree, sortie, cmd, nbacte, nbdas, severite, age, typeghm, duree_sej, valo=valopmctmonotime1, supplements=rec_sup_repa)
      datapca1 <- prcomp(na.omit(pc[ , apply(pc, 2, var) != 0]), center = TRUE, scale = TRUE)
      # summary(datapca1)
      
      output$anavalo9 <- renderPlotly({
        ggplotly(ggbiplot(pcobj = datapca1, obs.scale=1, scale = 1, alpha = 0, varname.size = 5, varname.adjust = TRUE, ellipse = TRUE) + geom_point(aes(color=pc$annee, text=paste("cmd: ",pc$cmd, '</br>dmr: ', pc$dmr, '</br>nbrum: ', pc$nbrum_sej, '</br>valo: ', round(pc$valo,1))), show.legend = FALSE)) %>% layout(titlefont=list(size=22))
      })
      output$anavalo10 <- renderPlotly({
        ggplotly(ggbiplot(pcobj = datapca1, choices=c(3,4), obs.scale=1, scale = 1, alpha = 0, varname.size = 5, varname.adjust = TRUE, ellipse = TRUE) + geom_point(aes(color=pc$annee, text=paste("cmd: ",pc$cmd, '</br>dmr: ', pc$dmr, '</br>nbrum: ', pc$nbrum_sej, '</br>valo: ', round(pc$valo,1))), show.legend = FALSE)) %>% layout(titlefont=list(size=22))
      })
    } 
  })
  
  onStop(session = NULL, fun = function() {
    rm(anno)
    rm(mese)
    rm(anonconsol)
    rm(mnonconsol)
    rm(naselect)
    rm(pmctmono)
    # rm(pmctmono_tarifsante)
    # rm(pmctmono_nonconsol)
    rm(rsa)
    rm(rum)
    rm(structures)
    rm(tarifs)
    rm(unita)
    rm(unitemed)
  } )
  
  session$onSessionEnded(function() {
    rm(anno)
    rm(mese)
    rm(anonconsol)
    rm(mnonconsol)
    rm(naselect)
    rm(pmctmono)
    # rm(pmctmono_tarifsante)
    # rm(pmctmono_nonconsol)
    rm(rsa)
    rm(rum)
    rm(structures)
    rm(tarifs)
    rm(unita)
    rm(unitemed)
  })
  
  
}

