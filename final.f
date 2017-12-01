C                       Trabalho de Simula‡Æo estoc stica
C        Alunos:
C              Victor Henrique Ribeiro
C              Tadeu Martines
C              Matheus Gaseta

C      Declara‡Æo de Vari veis
C      --
       Real*8 rnum(10000),dmax, pmod,sor,prop,somatsis,mediasis,propcli
       Real*8 tec, ts, treal, tini, tfim, tfila, tsis, npss, tlivre
       Real*8 vetor(10000), p_caixalivre, t_total, t_caixalivre,somatec
       Real*8 mediatec,somats,mediats,somatfila,mediafila,clientsoma
       Real*8 nmediatec,nmediats,nmediasis,nmediafila,varmediatec
       Real*8 varmediats, varmediasis, varmediafila
       Real*8 smq_tec, smq_ts, smq_fila, smq_sis
       Real*8 u_tec, u_ts, u_fila, u_sis!, nsem_real
       Integer ISEED, n, num, nsem
C      --
C      Declara‡Æo dos arquivos
C      --
       Open (1, file = 'tab.dat', status = 'unknown')
       Open (2, file = 'medias.dat', status = 'unknown')
       Open (3, file = 'teste.dat', status = 'unknown')
       Open (4, file = 'variancia.dat', status = 'unknown')
       Open (7, file = 'intervalo.dat', status = 'unknown')
C      --

C      Obten‡Æo de dados para execu‡Æo
       pmod = 2147483647.d0 ! 2**31-1
99     format(f18.7)
       write(*,80)
80     format('Valor inicial ou semente')  !Valor da semente
       read(*,92) ISEED
92     format(I5)
       write(*,81) ISEED
81     format('A semente eh: ', I8)
       write(*,82)
82     format('Entre com o numero a serem gerados') !Numeros a serem
                                                    !gerados a cada replica
       read(*,92) num
       write(*,98)
98     format('Entre com o numero de sementes') !Numero de replicas geradas
       read(*,92) nsem

C      Inicializa‡Æo

       tec = 0.0d0            !Tempo entre chegadas
       ts = 0.0d0             !Tempo de sa¡da
       treal= 0.0d0           !Contagem de em "tempo real"
                              !de execu‡Æo
       tini = 0.0d0           !Tempo de in¡cio
       tfim = 0.0d0           !Fim de atendimento
       tfila = 0.0d0          !Tempo de fila
       tsis = 0.0d0           !Tempo no sistema
       npss = 0.0d0           !Numero de pessoas
       tlivre = 0.0d0         !Tempo de caixa livre
       p_caixalivre = 0.0d0   !Probabilidade de caixa livre
       t_caixalivre = 0.0d0   !Tempo caixa livre
       somatec = 0.d0         !Soma de tempo entre chegadas
       mediatec = 0.d0        !M‚dia de tempo entre chegadas
       somats = 0.d0          !Soma de tempo no sistema
       mediats = 0.d0         !M‚dia de tempo entre chegadas
       somatfila = 0.d0       !Soma de tempo de fila
       mediafila = 0.d0       !M‚dia de tempo de fila
       somatsis = 0.d0        !Soma de tempo no sistema
       mediasis = 0.d0        !M‚dia de tempo no sistema
       propcli = 0.d0         !Probabilidade deumcliente chegar
       clientsoma = 0.d0      !Numero total de Clientes

       Do 1 i = 1, 10000
1         rnum(i) = 0.0d0     !Zeragem do vetor de numeros aleat¢rios

       Do 2 i = 1, 10000
2         vetor(i) = 0.d0     !Zeragem de vetor que armazena tempos
                              !finais para determinar numero de pessoas
                              !na fila

C      Calculos

       dmax = 1.0d0/pmod      !
       npss = 0.d0
       write(1,77)

 77    format(4x,'TEC',5x,'TS',6X,'TR',6X,'TINI',4X,'TFIM',4X,'TF',6X,
     *  'TSIS',4X,'NUMP',4X,'TLIVRE')
        Do k=1,nsem

             ISEED = ISEED + 2                  !Mudan‡a de semente
                                                !A cada ciclo
             tec = 0.0d0
             ts = 0.0d0
             treal= 0.0d0
             tini = 0.0d0
             tfim = 0.0d0
             tfila = 0.0d0
             tsis = 0.0d0                       !Zeragem de variaveis a
             npss = 0.0d0                       !Cada ciclo
             tlivre = 0.0d0
             p_caixalivre = 0.0d0
             t_caixalivre = 0.0d0
             somatec = 0.0d0
             somats = 0.0d0
             somatfila = 0.0d0
             somatsis = 0.0d0


             Do i=1,num

                rnum(i) = cong(ISEED)     !Chama a fun‡Æo que retorna
                                          !Um numero aleat¢rio
                !Dependendo do numero gerado define quando
                !O pr¢ximo cliente deve chegar
                if(rnum(i).gt.0.0d0.and.rnum(i).lt.0.52d0) tec = 1
                if(rnum(i).gt.0.52d0.and.rnum(i).lt.0.80d0) tec = 3
                if(rnum(i).gt.0.80d0.and.rnum(i).lt.0.90d0) tec = 5
                if(rnum(i).gt.0.90d0.and.rnum(i).lt.0.96d0) tec = 7
                if(rnum(i).gt.0.96d0) tec = 9

                !Dependendo do numero gerado define um tempo de servi‡o
                if(rnum(i).gt.0.0d0.and.rnum(i).lt.0.50d0) ts = 1.25d0
                if(rnum(i).gt.0.50d0.and.rnum(i).lt.0.82d0) ts = 3.75d0
                if(rnum(i).gt.0.82d0.and.rnum(i).lt.0.90d0) ts = 6.25d0
                if(rnum(i).gt.0.90d0.and.rnum(i).lt.0.94d0) ts = 8.75d0
                if(rnum(i).gt.0.94d0) ts = 11.25d0

                treal = treal + tec    !Soma o valor entre chegadas para
                                       !Obter o tempo real de chegada
                !Tempo final do anterior
                vetor(i) = tfim
                
                !Calcula o tempo inicial com base no tempo final
                if(tfim.gt.treal) then
                    tini = tfim
                else if(tfim.le.treal) then
                    tini = treal
                end if
                !Atualiza os tempos de fila,sistema,livre, fim com base
                !Do cliente anterior
                tfila = tini - treal
                tsis = tfila + ts
                tlivre = tini - tfim
                tfim = tini + ts
                t_caixalivre = t_caixalivre + tlivre
                
                somatec = somatec + tec
                somats = somats + ts             !Soma os valores para
                somatfila = somatfila + tfila    !as medias
                somatsis = somatsis + tsis

                npss = 0.d0 !zera o numero de pessoas na fila

                !Recalcula o numero de pessoas na fila pelo tempo de fim
                !Com o tempo real
                do j=0,num
                   if(vetor(j).gt.0.d0.and.vetor(j).lt.treal) then
                       npss = npss - 1.d0
                       vetor(j) = 0.d0
                   else if(vetor(j).gt.0.d0.and.vetor(j).gt.treal) then
                       npss = npss + 1.d0
                   end if
                   if(npss.lt.0) npss = 0

                end do
                !Escreve os valores calculados no arquivo tab.dat
                write(1,45)tec,ts,treal,tini,tfim,tfila,tsis,npss,tlivre

            end do

45         format(f8.2,f8.2,f8.2,F8.2,f8.2,f8.2,f8.2,f8.2,f8.2)
            !Probabilidade de caixa livre
            p_caixalivre = tfim / t_caixalivre
            !Probabilidade de chegada de clientes
            propcli = npss / num
            
            !Soma as medias para calcular media das medias
            !Media tempo entre chegadas
            mediatec = mediatec + (somatec / num)
            !Media tempo de servi‡o
            mediats = mediats + (somats / num)
            !Media tempo de fila
            mediafila = mediafila + (somatfila / num)
            !Media tempo de sistema
            mediasis = mediasis + (somatsis / num)


            !Soma a media ao quadrado para calcular a variƒncia
            smq_tec = smq_tec + ((somatec/num)**2)
            smq_ts = smq_ts + ((somats/num)**2)
            smq_fila = smq_fila + ((somatfila / num) ** 2)
            smq_sis = smq_sis + ((somatsis / num) ** 2)

        end do

        nmediasis = 0.d0
        nmediatec = 0.d0          !Zera os valores de media para o
        nmediats = 0.d0           !calculo
        nmediafila = 0.d0
        !Calculo de Media das Medias
        nmediatec = mediatec/nsem
        nmediats = mediats/nsem
        nmediafila = mediafila/nsem
        nmediasis = mediasis/nsem

        varmediatec = 0.d0
        varmediats = 0.d0         !Zera os valores de variancia para o
        varmediafila = 0.d0       !Calculo
        varmediasis = 0.d0
        !Calculo de Variancia
        varmediatec = (smq_tec-((mediatec*mediatec)/nsem))/(nsem-1)
        varmediats = (smq_ts-((mediats*mediats)/nsem))/(nsem-1)
        varmediafila = (smq_fila-((mediafila*mediafila)/nsem))/(nsem-1)
        varmediasis = (smq_sis-((mediasis*mediasis)/nsem))/(nsem-1)

        write(7,444)
444     format(20x,"Intervalo")
        write(7,196)
196     format(8x,"TEC",9x,"TS",9x,"TFILA",7x,"TSIS")

        !Calculo de Intervalo
        !Intervalo de Baixo
        u_tec =  nmediatec-1.96*(sqrt(varmediatec)/sqrt(REAL(nsem)))
        u_ts =  nmediats-1.96*(sqrt(varmediats)/sqrt(REAL(nsem)))
        u_fila =  nmediafila-1.96*(sqrt(varmediafila)/sqrt(REAL(nsem)))
        u_sis =  nmediasis-1.96*(sqrt(varmediasis)/sqrt(REAL(nsem)))

        write(7,100) u_tec, u_ts, u_fila, u_sis !Escrita no txt
                                               !de Intervalo.dat
        !Intervalo de Cima
        u_tec =  nmediatec+1.96*(sqrt(varmediatec)/sqrt(REAL(nsem)))
        u_ts =  nmediats+1.96*(sqrt(varmediats)/sqrt(REAL(nsem)))
        u_fila =  nmediafila+1.96*(sqrt(varmediafila)/sqrt(REAL(nsem)))
        u_sis =  nmediasis+1.96*(sqrt(varmediasis)/sqrt(REAL(nsem)))

        write(7,79) u_tec, u_ts, u_fila, u_sis !Escrita no txt de
                                               !Intervalo.dat
        !Escrita das Medias no arquivo
        write(2,155)
155     format(19x,"Medias")
        write(2,156)
156     format(3x,"TEC",9x,"TS",9x,"TFILA",7x,"TSIS")
        write(2,78) nmediatec,nmediats,nmediafila,nmediasis
        
        !Escrita da Variancia no arquivo
        write(4,177)
177     format(15x,"Variancia")
        write(4,178)
178     format(3x,"TEC",9x,"TS",9x,"TFILA",7x,"TSIS")
        write(4,78) varmediatec,varmediats,varmediafila,varmediasis

        !Formatos de escrita das variaveis
78      format(f12.7,f12.7,f12.7,f12.7)
100     format("Baixo",f12.7,f12.7,f12.7,f12.7)
79      format("Cima",1x,f12.7,f12.7,f12.7,f12.7)
76      format(f18.7, f18.7)


       close(1)

       end

       FUNCTION cong(ISEED)                !Fun‡Æo de gera‡Æo de numeros
                                           !Aleat¢rios
              REAL*8 rmod, pmod, dmax
              Integer ISEED, IMOD

              RMOD = DFLOAT(ISEED)
              PMOD = 2147483647.0D0
              dmax = 1.0d0/pmod
              rmod = rmod * 16807.0d0
              IMOD = RMOD * dmax
              RMOD = RMOD - PMOD*IMOD
              CONG = RMOD * dmax
              ISEED = RMOD
       Return
       end

