## Calculations

**Target Achievement**
  - Data Source: MSD
  - FY Cumulative Results / FY Targets

**HIV Prevalence**
  - Data Source: MSD
  - PLHIV / Population

**OVC Coverage of CLHIV**
  - Data Sources: MSD + NAT_SUBNAT MSD
  - OVC_HIVSTAT_POS (Receiving ART, <15) / PLHIV (<15)
    - Only in OVC Programed PSNUs 
    
**OVC Coverage of Children on Treatment**
  - Data Sources: MSD
  - OVC_HIVSTAT_POS (Receiving ART, <15) / TX_CURR (Age/Sex/HIVStatus, <15)
    - Only in OVC Programed PSNUs 
    
**OVC Programmed PSNU**
  - Data Source: MSD
  - PSNUs where there is a OVC programming - OVC_HIVSTAT Total Numerator > 0
  
**Proxy Linkage**
  - Data Source: MSD
  - TX_NEW / HTS_TST_POS
  
**Undiagnosed**
  - Data Source: UNAIDS
  - PLHIV - PLHIV with Known Status

**Treatment Coverage Status**
  - Data Source: NAT_SUBNAT MSD
  - Diagnosed / PLHIV

**Treatment Gap**
  - Data Source: NAT_SUBNAT MSD
  - TX_CURR_SUBNAT / PLHIV

**Viral Load Coverage**
  - Data Source: MSD
  - TX_PVLS_D / TX_CURR (2 periods prior)
  
**Viral Load Suppression**
  - Data Source: MSD
  - TX_PVLS / TX_PVLS_D


## Adjustments

**AGYW Grouping**
  - Data Source: MSD
  - 10-24 Females 
  
**GenPop Grouping**
  - Data Source: MSD
  - Total Numerator - KeyPoP (min = 0)
  
**Modality Groupings**
  - Data Source: MSD
  - Prevention = PMTCT ANC, Post ANC1, VMMC
  - Case Finding = All other modalities
  
**Remove Known Issues**
  - Data Sources: MSD and FSD
  - Removes all flagged known issues flagged by SI and ER country backstops
  - Source doc: https://docs.google.com/spreadsheets/d/1CMPY-GCWP3NSNWvLGLwMwBaPZsobgvjtobhjkZkDfow/edit#gid=1630220016
  - Source owners: OHA/SIEI/SI
  
**Remove M&O**
  - Data Source: FSD
  - Keep only where record_type == "Implementing Mechanism"
  
**Remove SCH**
  - Data Source: FSD
  - Remove all SCH and SGAC flagged supply chain mechanisms
  - Source doc: https://docs.google.com/spreadsheets/d/1mCJWDo4FPW2cQ6LpbsSjtnRjT7sUpPEOqxfT2zQNo64/edit#gid=1369222419
  - Source owners: OHA/SIEI/ER
  
**Funding Type Groupings**
  - Data Sources: FSD and HRH SD
  - Site Level Non-SD = interaction_type either Non Service Delivery or Non-Service Delivery
  - Service Delivery = interaction_type == Direct Service Delivery
  - All others are their program type
  - 