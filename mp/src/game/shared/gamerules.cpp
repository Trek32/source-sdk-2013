//========= Copyright Valve Corporation, All rights reserved. ============//
//
// Purpose: 
//
//=============================================================================//

#include "cbase.h"
#include "gamerules.h"
#include "ammodef.h"
#include "tier0/vprof.h"
#include "KeyValues.h"
#include "iachievementmgr.h"

#ifdef CLIENT_DLL

	#include "usermessages.h"

#else

	#include "player.h"
	#include "teamplay_gamerules.h"
	#include "game.h"
	#include "entitylist.h"
	#include "basecombatweapon.h"
	#include "voice_gamemgr.h"
	#include "globalstate.h"
	#include "player_resource.h"
	#include "tactical_mission.h"
	#include "gamestats.h"

#endif

// memdbgon must be the last include file in a .cpp file!!!
#include "tier0/memdbgon.h"


ConVar g_Language( "g_Language", "0", FCVAR_REPLICATED );
ConVar sk_autoaim_mode( "sk_autoaim_mode", "1", FCVAR_ARCHIVE | FCVAR_REPLICATED );

#ifndef CLIENT_DLL
ConVar log_verbose_enable( "log_verbose_enable", "0", FCVAR_GAMEDLL, "Set to 1 to enable verbose server log on the server." );
ConVar log_verbose_interval( "log_verbose_interval", "3.0", FCVAR_GAMEDLL, "Determines the interval (in seconds) for the verbose server log." );
#endif // CLIENT_DLL

static CViewVectors g_DefaultViewVectors(
	Vector( 0, 0, 64 ),			//VEC_VIEW (m_vView)
								
	Vector(-16, -16, 0 ),		//VEC_HULL_MIN (m_vHullMin)
	Vector( 16,  16,  72 ),		//VEC_HULL_MAX (m_vHullMax)
													
	Vector(-16, -16, 0 ),		//VEC_DUCK_HULL_MIN (m_vDuckHullMin)
	Vector( 16,  16,  36 ),		//VEC_DUCK_HULL_MAX	(m_vDuckHullMax)
	Vector( 0, 0, 28 ),			//VEC_DUCK_VIEW		(m_vDuckView)
													
	Vector(-10, -10, -10 ),		//VEC_OBS_HULL_MIN	(m_vObsHullMin)
	Vector( 10,  10,  10 ),		//VEC_OBS_HULL_MAX	(m_vObsHullMax)
													
	Vector( 0, 0, 14 )			//VEC_DEAD_VIEWHEIGHT (m_vDeadViewHeight)
);													
													

// ------------------------------------------------------------------------------------ //
// CGameRulesProxy implementation.
// ------------------------------------------------------------------------------------ //

CGameRulesProxy *CGameRulesProxy::s_pGameRulesProxy = NULL;

IMPLEMENT_NETWORKCLASS_ALIASED( GameRulesProxy, DT_GameRulesProxy )

// Don't send any of the CBaseEntity stuff..
BEGIN_NETWORK_TABLE_NOBASE( CGameRulesProxy, DT_GameRulesProxy )
END_NETWORK_TABLE()


CGameRulesProxy::CGameRulesProxy()
{
	// allow map placed proxy entities to overwrite the static one
	if ( s_pGameRulesProxy )
	{
#ifndef CLIENT_DLL
		UTIL_Remove( s_pGameRulesProxy );
#endif
		s_pGameRulesProxy = NULL;
	}
	s_pGameRulesProxy = this;
}

CGameRulesProxy::~CGameRulesProxy()
{
	if ( s_pGameRulesProxy == this )
	{
		s_pGameRulesProxy = NULL;
	}
}

int CGameRulesProxy::UpdateTransmitState()
{
#ifndef CLIENT_DLL
	// ALWAYS transmit to all clients.
	return SetTransmitState( FL_EDICT_ALWAYS );
#else
	return 0;
#endif

}

void CGameRulesProxy::NotifyNetworkStateChanged()
{
	if ( s_pGameRulesProxy )
		s_pGameRulesProxy->NetworkStateChanged();
}



ConVar	old_radius_damage( "old_radiusdamage", "0.0", FCVAR_REPLICATED );

#ifdef CLIENT_DLL //{

bool CGameRules::IsBonusChallengeTimeBased( void )
{
	return true;
}

bool CGameRules::IsLocalPlayer( int nEntIndex )
{
	C_BasePlayer *pLocalPlayer = C_BasePlayer::GetLocalPlayer();
	return ( pLocalPlayer && pLocalPlayer == ClientEntityList().GetEnt( nEntIndex ) );
}

CGameRules::CGameRules() : CAutoGameSystemPerFrame( "CGameRules" )
{
	Assert( !g_pGameRules );
	g_pGameRules = this;
}	

#else //}{

// In tf_gamerules.cpp or hl_gamerules.cpp.
extern IVoiceGameMgrHelper *g_pVoiceGameMgrHelper;


CGameRules*	g_pGameRules = NULL;
extern bool	g_fGameOver;

//-----------------------------------------------------------------------------
// constructor, destructor
//-----------------------------------------------------------------------------
CGameRules::CGameRules() : CAutoGameSystemPerFrame( "CGameRules" )
{
	Assert( !g_pGameRules );
	g_pGameRules = this;

	GetVoiceGameMgr()->Init( g_pVoiceGameMgrHelper, gpGlobals->maxClients );
	ClearMultiDamage();

	m_flNextVerboseLogOutput = 0.0f;
}

//-----------------------------------------------------------------------------
// Purpose: Return true if the specified player can carry any more of the ammo type
//-----------------------------------------------------------------------------
bool CGameRules::CanHaveAmmo( CBaseCombatCharacter *pPlayer, int iAmmoIndex )
{
	if ( iAmmoIndex > -1 )
	{
		// Get the max carrying capacity for this ammo
		int iMaxCarry = GetAmmoDef()->MaxCarry( iAmmoIndex );

		// Does the player have room for more of this type of ammo?
		if ( pPlayer->GetAmmoCount( iAmmoIndex ) < iMaxCarry )
			return true;
	}

	return false;
}

//-----------------------------------------------------------------------------
// Purpose: Return true if the specified player can carry any more of the ammo type
//-----------------------------------------------------------------------------
bool CGameRules::CanHaveAmmo( CBaseCombatCharacter *pPlayer, const char *szName )
{
	return CanHaveAmmo( pPlayer, GetAmmoDef()->Index(szName) );
}

//=========================================================
//=========================================================
CBaseEntity *CGameRules::GetPlayerSpawnSpot( CBasePlayer *pPlayer )
{
	CBaseEntity *pSpawnSpot = pPlayer->EntSelectSpawnPoint();
	Assert( pSpawnSpot );

	pPlayer->SetLocalOrigin( pSpawnSpot->GetAbsOrigin() + Vector(0,0,1) );
	pPlayer->SetAbsVelocity( vec3_origin );
	pPlayer->SetLocalAngles( pSpawnSpot->GetLocalAngles() );
	pPlayer->m_Local.m_vecPunchAngle = vec3_angle;
	pPlayer->m_Local.m_vecPunchAngleVel = vec3_angle;
	pPlayer->SnapEyeAngles( pSpawnSpot->GetLocalAngles() );

	return pSpawnSpot;
}

// checks if the spot is clear of players
bool CGameRules::IsSpawnPointValid( CBaseEntity *pSpot, CBasePlayer *pPlayer  )
{
	CBaseEntity *ent = NULL;

	if ( !pSpot->IsTriggered( pPlayer ) )
	{
		return false;
	}

	for ( CEntitySphereQuery sphere( pSpot->GetAbsOrigin(), 128 ); (ent = sphere.GetCurrentEntity()) != NULL; sphere.NextEntity() )
	{
		// if ent is a client, don't spawn on 'em
		if ( ent->IsPlayer() && ent != pPlayer )
			return false;
	}

	return true;
}

//=========================================================
//=========================================================
bool CGameRules::CanHavePlayerItem( CBasePlayer *pPlayer, CBaseCombatWeapon *pWeapon )
{
/*
	if ( pWeapon->m_pszAmmo1 )
	{
		if ( !CanHaveAmmo( pPlayer, pWeapon->m_iPrimaryAmmoType ) )
		{
			// we can't carry anymore ammo for this gun. We can only 
			// have the gun if we aren't already carrying one of this type
			if ( pPlayer->Weapon_OwnsThisType( pWeapon ) )
			{
				return FALSE;
			}
		}
	}
	else
	{
		// weapon doesn't use ammo, don't take another if you already have it.
		if ( pPlayer->Weapon_OwnsThisType( pWeapon ) )
		{
			return FALSE;
		}
	}
*/
	// note: will fall through to here if GetItemInfo doesn't fill the struct!
	return TRUE;
}

//=========================================================
// load the SkillData struct with the proper values based on the skill level.
//=========================================================
void CGameRules::RefreshSkillData ( bool forceUpdate )
{
#ifndef CLIENT_DLL
	if ( !forceUpdate )
	{
		if ( GlobalEntity_IsInTable( "skill.cfg" ) )
			return;
	}
	GlobalEntity_Add( "skill.cfg", STRING(gpGlobals->mapname), GLOBAL_ON );

#if !defined( TF_DLL ) && !defined( DOD_DLL )
	char	szExec[256];
#endif 

	ConVarRef skill( "skill" );

	SetSkillLevel( skill.IsValid() ? skill.GetInt() : 1 );

#ifdef HL2_DLL
	// HL2 current only uses one skill config file that represents MEDIUM skill level and
	// synthesizes EASY and HARD. (sjb)
	Q_snprintf( szExec,sizeof(szExec), "exec skill_manifest.cfg\n" );

	engine->ServerCommand( szExec );
	engine->ServerExecute();
#else

#if !defined( TF_DLL ) && !defined( DOD_DLL )
	Q_snprintf( szExec,sizeof(szExec), "exec skill%d.cfg\n", GetSkillLevel() );

	engine->ServerCommand( szExec );
	engine->ServerExecute();
#endif // TF_DLL && DOD_DLL

#endif // HL2_DLL
#endif // CLIENT_DLL
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
bool IsExplosionTraceBlocked( trace_t *ptr )
{
	if( ptr->DidHitWorld() )
		return true;

	if( ptr->m_pEnt == NULL )
		return false;

	if( ptr->m_pEnt->GetMoveType() == MOVETYPE_PUSH )
	{
		// All doors are push, but not all things that push are doors. This 
		// narrows the search before we start to do classname compares.
		if( FClassnameIs(ptr->m_pEnt, "prop_door_rotating") ||
        FClassnameIs(ptr->m_pEnt, "func_door") ||
        FClassnameIs(ptr->m_pEnt, "func_door_rotating") )
			return true;
	}

	return false;
}

//-----------------------------------------------------------------------------
// Default implementation of radius damage
//-----------------------------------------------------------------------------
#define ROBUST_RADIUS_PROBE_DIST 16.0f // If a solid surface blocks the explosion, this is how far to creep along the surface looking for another way to the target
void CGameRules::RadiusDamage( const CTakeDamageInfo &info, const Vector &vecSrcIn, float flRadius, int iClassIgnore, CBaseEntity *pEntityIgnore )
{
	const int MASK_RADIUS_DAMAGE = MASK_SHOT&(~CONTENTS_HITBOX);
	CBaseEntity *pEntity = NULL;
	trace_t		tr;
	float		flAdjustedDamage, falloff;
	Vector		vecSpot;

	Vector vecSrc = vecSrcIn;

	if ( flRadius )
		falloff = info.GetDamage() / flRadius;
	else
		falloff = 1.0;

	int bInWater = (UTIL_PointContents ( vecSrc ) & MASK_WATER) ? true : false;

#ifdef HL2_DLL
	if( bInWater )
	{
		// Only muffle the explosion if deeper than 2 feet in water.
		if( !(UTIL_PointContents(vecSrc + Vector(0, 0, 24)) & MASK_WATER) )
		{
			bInWater = false;
		}
	}
#endif // HL2_DLL
	
	vecSrc.z += 1;// in case grenade is lying on the ground

	float flHalfRadiusSqr = Square( flRadius / 2.0f );

	// iterate on all entities in the vicinity.
	for ( CEntitySphereQuery sphere( vecSrc, flRadius ); (pEntity = sphere.GetCurrentEntity()) != NULL; sphere.NextEntity() )
	{
		// This value is used to scale damage when the explosion is blocked by some other object.
		float flBlockedDamagePercent = 0.0f;

		if ( pEntity == pEntityIgnore )
			continue;

		if ( pEntity->m_takedamage == DAMAGE_NO )
			continue;

		// UNDONE: this should check a damage mask, not an ignore
		if ( iClassIgnore != CLASS_NONE && pEntity->Classify() == iClassIgnore )
		{// houndeyes don't hurt other houndeyes with their attack
			continue;
		}

		// blast's don't tavel into or out of water
		if (bInWater && pEntity->GetWaterLevel() == 0)
			continue;

		if (!bInWater && pEntity->GetWaterLevel() == 3)
			continue;

		// Check that the explosion can 'see' this entity.
		vecSpot = pEntity->BodyTarget( vecSrc, false );
		UTIL_TraceLine( vecSrc, vecSpot, MASK_RADIUS_DAMAGE, info.GetInflictor(), COLLISION_GROUP_NONE, &tr );

		if( old_radius_damage.GetBool() )
		{
			if ( tr.fraction != 1.0 && tr.m_pEnt != pEntity )
			continue;
		}
		else
		{
			if ( tr.fraction != 1.0 )
			{
				if ( IsExplosionTraceBlocked(&tr) )
				{
					if( ShouldUseRobustRadiusDamage( pEntity ) )
					{
						if( vecSpot.DistToSqr( vecSrc ) > flHalfRadiusSqr )
						{
							// Only use robust model on a target within one-half of the explosion's radius.
							continue;
						}

						Vector vecToTarget = vecSpot - tr.endpos;
						VectorNormalize( vecToTarget );

						// We're going to deflect the blast along the surface that 
						// interrupted a trace from explosion to this target.
						Vector vecUp, vecDeflect;
						CrossProduct( vecToTarget, tr.plane.normal, vecUp );
						CrossProduct( tr.plane.normal, vecUp, vecDeflect );
						VectorNormalize( vecDeflect );

						// Trace along the surface that intercepted the blast...
						UTIL_TraceLine( tr.endpos, tr.endpos + vecDeflect * ROBUST_RADIUS_PROBE_DIST, MASK_RADIUS_DAMAGE, info.GetInflictor(), COLLISION_GROUP_NONE, &tr );
						//NDebugOverlay::Line( tr.startpos, tr.endpos, 255, 255, 0, false, 10 );

						// ...to see if there's a nearby edge that the explosion would 'spill over' if the blast were fully simulated.
						UTIL_TraceLine( tr.endpos, vecSpot, MASK_RADIUS_DAMAGE, info.GetInflictor(), COLLISION_GROUP_NONE, &tr );
						//NDebugOverlay::Line( tr.startpos, tr.endpos, 255, 0, 0, false, 10 );

						if( tr.fraction != 1.0 && tr.DidHitWorld() )
						{
							// Still can't reach the target.
							continue;
						}
						// else fall through
					}
					else
					{
						continue;
					}
				}

				// UNDONE: Probably shouldn't let children block parents either?  Or maybe those guys should set their owner if they want this behavior?
				// HL2 - Dissolve damage is not reduced by interposing non-world objects
				if( tr.m_pEnt && tr.m_pEnt != pEntity && tr.m_pEnt->GetOwnerEntity() != pEntity )
				{
					// Some entity was hit by the trace, meaning the explosion does not have clear
					// line of sight to the entity that it's trying to hurt. If the world is also
					// blocking, we do no damage.
					CBaseEntity *pBlockingEntity = tr.m_pEnt;
					//Msg( "%s may be blocked by %s...", pEntity->GetClassname(), pBlockingEntity->GetClassname() );

					UTIL_TraceLine( vecSrc, vecSpot, CONTENTS_SOLID, info.GetInflictor(), COLLISION_GROUP_NONE, &tr );

					if( tr.fraction != 1.0 )
					{
						continue;
					}
					
					// Now, if the interposing object is physics, block some explosion force based on its mass.
					if( pBlockingEntity->VPhysicsGetObject() )
					{
						const float MASS_ABSORB_ALL_DAMAGE = 350.0f;
						float flMass = pBlockingEntity->VPhysicsGetObject()->GetMass();
						float scale = flMass / MASS_ABSORB_ALL_DAMAGE;

						// Absorbed all the damage.
						if( scale >= 1.0f )
						{
							continue;
						}

						ASSERT( scale > 0.0f );
						flBlockedDamagePercent = scale;
						//Msg("  Object (%s) weighing %fkg blocked %f percent of explosion damage\n", pBlockingEntity->GetClassname(), flMass, scale * 100.0f);
					}
					else
					{
						// Some object that's not the world and not physics. Generically block 25% damage
						flBlockedDamagePercent = 0.25f;
					}
				}
			}
		}
		// decrease damage for an ent that's farther from the bomb.
		flAdjustedDamage = ( vecSrc - tr.endpos ).Length() * falloff;
		flAdjustedDamage = info.GetDamage() - flAdjustedDamage;

		if ( flAdjustedDamage <= 0 )
		{
			continue;
		}

		// the explosion can 'see' this entity, so hurt them!
		if (tr.startsolid)
		{
			// if we're stuck inside them, fixup the position and distance
			tr.endpos = vecSrc;
			tr.fraction = 0.0;
		}
		
		CTakeDamageInfo adjustedInfo = info;
		//Msg("%s: Blocked damage: %f percent (in:%f  out:%f)\n", pEntity->GetClassname(), flBlockedDamagePercent * 100, flAdjustedDamage, flAdjustedDamage - (flAdjustedDamage * flBlockedDamagePercent) );
		adjustedInfo.SetDamage( flAdjustedDamage - (flAdjustedDamage * flBlockedDamagePercent) );

		// Now make a consideration for skill level!
		if( info.GetAttacker() && info.GetAttacker()->IsPlayer() && pEntity->IsNPC() )
		{
			// An explosion set off by the player is harming an NPC. Adjust damage accordingly.
			adjustedInfo.AdjustPlayerDamageInflictedForSkillLevel();
		}

		Vector dir = vecSpot - vecSrc;
		VectorNormalize( dir );

		// If we don't have a damage force, manufacture one
		if ( adjustedInfo.GetDamagePosition() == vec3_origin || adjustedInfo.GetDamageForce() == vec3_origin )
		{
			if ( !( adjustedInfo.GetDamageType() & DMG_PREVENT_PHYSICS_FORCE ) )
			{
				CalculateExplosiveDamageForce( &adjustedInfo, dir, vecSrc );
			}
		}
		else
		{
			// Assume the force passed in is the maximum force. Decay it based on falloff.
			float flForce = adjustedInfo.GetDamageForce().Length() * falloff;
			adjustedInfo.SetDamageForce( dir * flForce );
			adjustedInfo.SetDamagePosition( vecSrc );
		}

		if ( tr.fraction != 1.0 && pEntity == tr.m_pEnt )
		{
			ClearMultiDamage( );
			pEntity->DispatchTraceAttack( adjustedInfo, dir, &tr );
			ApplyMultiDamage();
		}
		else
		{
			pEntity->TakeDamage( adjustedInfo );
		}

		// Now hit all triggers along the way that respond to damage... 
		pEntity->TraceAttackToTriggers( adjustedInfo, vecSrc, tr.endpos, dir );

#if defined( GAME_DLL )
		if ( info.GetAttacker() && info.GetAttacker()->IsPlayer() && ToBaseCombatCharacter( tr.m_pEnt ) )
		{

			// This is a total hack!!!
			bool bIsPrimary = true;
			CBasePlayer *player = ToBasePlayer( info.GetAttacker() );
			CBaseCombatWeapon *pWeapon = player->GetActiveWeapon();
			if ( pWeapon && FClassnameIs( pWeapon, "weapon_smg1" ) )
			{
				bIsPrimary = false;
			}

			gamestats->Event_WeaponHit( player, bIsPrimary, (pWeapon != NULL) ? player->GetActiveWeapon()->GetClassname() : "NULL", info );
		}
#endif
	}
}


bool CGameRules::ClientCommand( CBaseEntity *pEdict, const CCommand &args )
{
	if( pEdict->IsPlayer() )
	{
		if( GetVoiceGameMgr()->ClientCommand( static_cast<CBasePlayer*>(pEdict), args ) )
			return true;
	}

	return false;
}


void CGameRules::FrameUpdatePostEntityThink()
{
	VPROF( "CGameRules::FrameUpdatePostEntityThink" );
	Think();
}

// Hook into the convar from the engine
ConVar skill( "skill", "1" );

void CGameRules::Think()
{
	GetVoiceGameMgr()->Update( gpGlobals->frametime );
	SetSkillLevel( skill.GetInt() );

	if ( log_verbose_enable.GetBool() )
	{
		if ( m_flNextVerboseLogOutput < gpGlobals->curtime )
		{
			ProcessVerboseLogOutput();
			m_flNextVerboseLogOutput = gpGlobals->curtime + log_verbose_interval.GetFloat();
		}
	}
}

//-----------------------------------------------------------------------------
// Purpose: Called at the end of GameFrame (i.e. after all game logic has run this frame)
//-----------------------------------------------------------------------------
void CGameRules::EndGameFrame( void )
{
	// If you hit this assert, it means something called AddMultiDamage() and didn't ApplyMultiDamage().
	// The g_MultiDamage.m_hAttacker & g_MultiDamage.m_hInflictor should give help you figure out the culprit.
	Assert( g_MultiDamage.IsClear() );
	if ( !g_MultiDamage.IsClear() )
	{
		Warning("Unapplied multidamage left in the system:\nTarget: %s\nInflictor: %s\nAttacker: %s\nDamage: %.2f\n", 
			g_MultiDamage.GetTarget()->GetDebugName(),
			g_MultiDamage.GetInflictor()->GetDebugName(),
			g_MultiDamage.GetAttacker()->GetDebugName(),
			g_MultiDamage.GetDamage() );
		ApplyMultiDamage();
	}
}

//-----------------------------------------------------------------------------
// trace line rules
//-----------------------------------------------------------------------------
float CGameRules::WeaponTraceEntity( CBaseEntity *pEntity, const Vector &vecStart, const Vector &vecEnd,
					 unsigned int mask, trace_t *ptr )
{
	UTIL_TraceEntity( pEntity, vecStart, vecEnd, mask, ptr );
	return 1.0f;
}


void CGameRules::CreateStandardEntities()
{
	g_pPlayerResource = (CPlayerResource*)CBaseEntity::Create( "player_manager", vec3_origin, vec3_angle );
	g_pPlayerResource->AddEFlags( EFL_KEEP_ON_RECREATE_ENTITIES );
}

//-----------------------------------------------------------------------------
// Purpose: Inform client(s) they can mark the indicated achievement as completed (SERVER VERSION)
// Input  : filter - which client(s) to send this to
//			iAchievementID - The enumeration value of the achievement to mark (see TODO:Kerry, what file will have the mod's achievement enum?) 
//-----------------------------------------------------------------------------
void CGameRules::MarkAchievement( IRecipientFilter& filter, char const *pchAchievementName )
{
	gamestats->Event_IncrementCountedStatistic( vec3_origin, pchAchievementName, 1.0f );

	IAchievementMgr *pAchievementMgr = engine->GetAchievementMgr();
	if ( !pAchievementMgr )
		return;
	pAchievementMgr->OnMapEvent( pchAchievementName );
}

#endif //} !CLIENT_DLL


// ----------------------------------------------------------------------------- //
// Shared CGameRules implementation.
// ----------------------------------------------------------------------------- //

CGameRules::~CGameRules()
{
	Assert( g_pGameRules == this );
	g_pGameRules = NULL;
}

bool CGameRules::SwitchToNextBestWeapon( CBaseCombatCharacter *pPlayer, CBaseCombatWeapon *pCurrentWeapon )
{
	return false;
}

CBaseCombatWeapon *CGameRules::GetNextBestWeapon( CBaseCombatCharacter *pPlayer, CBaseCombatWeapon *pCurrentWeapon )
{
	return NULL;
}

bool CGameRules::ShouldCollide( int collisionGroup0, int collisionGroup1 )
{
	if ( collisionGroup0 > collisionGroup1 )
	{
		// swap so that lowest is always first
		::V_swap(collisionGroup0,collisionGroup1);
	}

#ifndef HL2MP
	if ( (collisionGroup0 == COLLISION_GROUP_PLAYER || collisionGroup0 == COLLISION_GROUP_PLAYER_MOVEMENT) &&
		collisionGroup1 == COLLISION_GROUP_PUSHAWAY )
	{
		return false;
	}
#endif

	if ( collisionGroup0 == COLLISION_GROUP_DEBRIS && collisionGroup1 == COLLISION_GROUP_PUSHAWAY )
	{
		// let debris and multiplayer objects collide
		return true;
	}
	
	// --------------------------------------------------------------------------
	// NOTE: All of this code assumes the collision groups have been sorted!!!!
	// NOTE: Don't change their order without rewriting this code !!!
	// --------------------------------------------------------------------------

	// Don't bother if either is in a vehicle...
	if (( collisionGroup0 == COLLISION_GROUP_IN_VEHICLE ) || ( collisionGroup1 == COLLISION_GROUP_IN_VEHICLE ))
		return false;

	if ( ( collisionGroup1 == COLLISION_GROUP_DOOR_BLOCKER ) && ( collisionGroup0 != COLLISION_GROUP_NPC ) )
		return false;

	if ( ( collisionGroup0 == COLLISION_GROUP_PLAYER ) && ( collisionGroup1 == COLLISION_GROUP_PASSABLE_DOOR ) )
		return false;

	if ( collisionGroup0 == COLLISION_GROUP_DEBRIS || collisionGroup0 == COLLISION_GROUP_DEBRIS_TRIGGER )
	{
		// put exceptions here, right now this will only collide with COLLISION_GROUP_NONE
		return false;
	}

	// Dissolving guys only collide with COLLISION_GROUP_NONE
	if ( (collisionGroup0 == COLLISION_GROUP_DISSOLVING) || (collisionGroup1 == COLLISION_GROUP_DISSOLVING) )
	{
		if ( collisionGroup0 != COLLISION_GROUP_NONE )
			return false;
	}

	// doesn't collide with other members of this group
	// or debris, but that's handled above
	if ( collisionGroup0 == COLLISION_GROUP_INTERACTIVE_DEBRIS && collisionGroup1 == COLLISION_GROUP_INTERACTIVE_DEBRIS )
		return false;

#ifndef HL2MP
	// This change was breaking HL2DM
	// Adrian: TEST! Interactive Debris doesn't collide with the player.
	if ( collisionGroup0 == COLLISION_GROUP_INTERACTIVE_DEBRIS && ( collisionGroup1 == COLLISION_GROUP_PLAYER_MOVEMENT || collisionGroup1 == COLLISION_GROUP_PLAYER ) )
		 return false;
#endif

	if ( collisionGroup0 == COLLISION_GROUP_BREAKABLE_GLASS && collisionGroup1 == COLLISION_GROUP_BREAKABLE_GLASS )
		return false;

	// interactive objects collide with everything except debris & interactive debris
	if ( collisionGroup1 == COLLISION_GROUP_INTERACTIVE && collisionGroup0 != COLLISION_GROUP_NONE )
		return false;

	// Projectiles hit everything but debris, weapons, + other projectiles
	if ( collisionGroup1 == COLLISION_GROUP_PROJECTILE )
	{
		if ( collisionGroup0 == COLLISION_GROUP_DEBRIS || 
			collisionGroup0 == COLLISION_GROUP_WEAPON ||
			collisionGroup0 == COLLISION_GROUP_PROJECTILE )
		{
			return false;
		}
	}

	// Don't let vehicles collide with weapons
	// Don't let players collide with weapons...
	// Don't let NPCs collide with weapons
	// Weapons are triggers, too, so they should still touch because of that
	if ( collisionGroup1 == COLLISION_GROUP_WEAPON )
	{
		if ( collisionGroup0 == COLLISION_GROUP_VEHICLE || 
			collisionGroup0 == COLLISION_GROUP_PLAYER ||
			collisionGroup0 == COLLISION_GROUP_NPC )
		{
			return false;
		}
	}

	// collision with vehicle clip entity??
	if ( collisionGroup0 == COLLISION_GROUP_VEHICLE_CLIP || collisionGroup1 == COLLISION_GROUP_VEHICLE_CLIP )
	{
		// yes then if it's a vehicle, collide, otherwise no collision
		// vehicle sorts lower than vehicle clip, so must be in 0
		if ( collisionGroup0 == COLLISION_GROUP_VEHICLE )
			return true;
		// vehicle clip against non-vehicle, no collision
		return false;
	}

	return true;
}


const CViewVectors* CGameRules::GetViewVectors() const
{
	return &g_DefaultViewVectors;
}


//-----------------------------------------------------------------------------
// Purpose: Returns how much damage the given ammo type should do to the victim
//			when fired by the attacker.
// Input  : pAttacker - Dude what shot the gun.
//			pVictim - Dude what done got shot.
//			nAmmoType - What been shot out.
// Output : How much hurt to put on dude what done got shot (pVictim).
//-----------------------------------------------------------------------------
float CGameRules::GetAmmoDamage( CBaseEntity *pAttacker, CBaseEntity *pVictim, int nAmmoType )
{
	float flDamage = 0;
	CAmmoDef *pAmmoDef = GetAmmoDef();

	if ( pAttacker->IsPlayer() )
	{
		flDamage = pAmmoDef->PlrDamage( nAmmoType );
	}
	else
	{
		flDamage = pAmmoDef->NPCDamage( nAmmoType );
	}

	return flDamage;
}


#ifndef CLIENT_DLL
const char *CGameRules::GetChatPrefix( bool bTeamOnly, CBasePlayer *pPlayer )
{
	if ( pPlayer && pPlayer->IsAlive() == false )
	{
		if ( bTeamOnly )
			return "*DEAD*(TEAM)";
		else
			return "*DEAD*";
	}
	
	return "";
}

void CGameRules::CheckHaptics(CBasePlayer* pPlayer)
{
	// NVNT see if the client of pPlayer is using a haptic device.
	const char *pszHH = engine->GetClientConVarValue( pPlayer->entindex(), "hap_HasDevice" );
	if( pszHH )
	{
		int iHH = atoi( pszHH );
		pPlayer->SetHaptics( iHH != 0 );
	}
}

void CGameRules::ClientSettingsChanged( CBasePlayer *pPlayer )
{
	const char *pszName = engine->GetClientConVarValue( pPlayer->entindex(), "name" );

	const char *pszOldName = pPlayer->GetPlayerName();

	// msg everyone if someone changes their name,  and it isn't the first time (changing no name to current name)
	// Note, not using FStrEq so that this is case sensitive
	if ( pszOldName[0] != 0 && Q_strcmp( pszOldName, pszName ) )
	{
		char text[256];
		Q_snprintf( text,sizeof(text), "%s changed name to %s\n", pszOldName, pszName );

		UTIL_ClientPrintAll( HUD_PRINTTALK, text );

		IGameEvent * event = gameeventmanager->CreateEvent( "player_changename" );
		if ( event )
		{
			event->SetInt( "userid", pPlayer->GetUserID() );
			event->SetString( "oldname", pszOldName );
			event->SetString( "newname", pszName );
			gameeventmanager->FireEvent( event );
		}
		
		pPlayer->SetPlayerName( pszName );
	}

	const char *pszFov = engine->GetClientConVarValue( pPlayer->entindex(), "fov_desired" );
	if ( pszFov )
	{
		int iFov = atoi(pszFov);
		iFov = clamp( iFov, 75, 90 );
		pPlayer->SetDefaultFOV( iFov );
	}

	// NVNT see if this user is still or has began using a haptic device
	const char *pszHH = engine->GetClientConVarValue( pPlayer->entindex(), "hap_HasDevice" );
	if( pszHH )
	{
		int iHH = atoi( pszHH );
		pPlayer->SetHaptics( iHH != 0 );
	}
}

void CGameRules::InitDefaultAIRelationships()
{
	//  Allocate memory for default relationships
	CBaseCombatCharacter::AllocateDefaultRelationships();

	// --------------------------------------------------------------
	// First initialize table so we can report missing relationships
	// --------------------------------------------------------------
	for( int i = 0; i < NUM_AI_CLASSES; i++ )
		for( int j = 0; j < NUM_AI_CLASSES; j++ )
			// By default all relationships are neutral of priority zero
			CBaseCombatCharacter::SetDefaultRelationship( ( Class_T )i, ( Class_T )j, D_NU, 0 );

	// ------------------------------------------------------------
	//	> CLASS_ANTLION
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_PLAYER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_CITIZEN_PASSIVE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_CITIZEN_REBEL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_COMBINE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_COMBINE_HUNTER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_CONSCRIPT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_HEADCRAB, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_MANHACK, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_METROPOLICE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_MILITARY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_SCANNER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_STALKER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_VORTIGAUNT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_ZOMBIE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_PROTOSNIPER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_ANTLION, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_PLAYER_ALLY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_PLAYER_ALLY_VITAL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ANTLION, CLASS_HACKED_ROLLERMINE, D_HT, 0 );

	// ------------------------------------------------------------
	//	> CLASS_BARNACLE
	//
	//  In this case, the relationship D_HT indicates which characters
	//  the barnacle will try to eat.
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_PLAYER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_BARNACLE, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_CITIZEN_PASSIVE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_CITIZEN_REBEL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_COMBINE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_COMBINE_HUNTER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_CONSCRIPT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_HEADCRAB, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_MANHACK, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_METROPOLICE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_MILITARY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_STALKER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_VORTIGAUNT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_ZOMBIE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_EARTH_FAUNA, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_PLAYER_ALLY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_PLAYER_ALLY_VITAL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BARNACLE, CLASS_HACKED_ROLLERMINE, D_HT, 0 );

	// ------------------------------------------------------------
	//	> CLASS_BULLSEYE
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_PLAYER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_ANTLION, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_CITIZEN_PASSIVE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_CITIZEN_REBEL, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_COMBINE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_COMBINE_HUNTER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_CONSCRIPT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_HEADCRAB, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_MANHACK, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_METROPOLICE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_MILITARY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_STALKER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_VORTIGAUNT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_ZOMBIE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_PLAYER_ALLY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_PLAYER_ALLY_VITAL, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_BULLSEYE, CLASS_HACKED_ROLLERMINE, D_NU, 0 );

	// ------------------------------------------------------------
	//	> CLASS_CITIZEN_PASSIVE
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_PLAYER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_BARNACLE, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_CITIZEN_PASSIVE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_CITIZEN_REBEL, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_COMBINE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_COMBINE_HUNTER, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_CONSCRIPT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_HEADCRAB, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_MANHACK, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_METROPOLICE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_MILITARY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_MISSILE, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_STALKER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_VORTIGAUNT, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_ZOMBIE, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_PLAYER_ALLY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_PLAYER_ALLY_VITAL, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_PASSIVE, CLASS_HACKED_ROLLERMINE, D_NU, 0 );

	// ------------------------------------------------------------
	//	> CLASS_CITIZEN_REBEL
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_PLAYER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_BARNACLE, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_CITIZEN_PASSIVE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_CITIZEN_REBEL, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_COMBINE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_COMBINE_HUNTER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_CONSCRIPT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_HEADCRAB, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_MANHACK, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_METROPOLICE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_MILITARY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_MISSILE, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_SCANNER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_STALKER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_VORTIGAUNT, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_ZOMBIE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_PLAYER_ALLY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_PLAYER_ALLY_VITAL, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CITIZEN_REBEL, CLASS_HACKED_ROLLERMINE, D_NU, 0 );

	// ------------------------------------------------------------
	//	> CLASS_COMBINE
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_PLAYER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_BARNACLE, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_CITIZEN_PASSIVE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_CITIZEN_REBEL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_COMBINE, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_COMBINE_GUNSHIP, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_COMBINE_HUNTER, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_CONSCRIPT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_HEADCRAB, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_MANHACK, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_METROPOLICE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_MILITARY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_STALKER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_VORTIGAUNT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_ZOMBIE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_PLAYER_ALLY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_PLAYER_ALLY_VITAL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE, CLASS_HACKED_ROLLERMINE, D_HT, 0 );

	// ------------------------------------------------------------
	//	> CLASS_COMBINE_GUNSHIP
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_PLAYER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_CITIZEN_PASSIVE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_CITIZEN_REBEL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_COMBINE, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_COMBINE_GUNSHIP, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_COMBINE_HUNTER, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_CONSCRIPT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_HEADCRAB, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_MANHACK, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_METROPOLICE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_MILITARY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_MISSILE, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_STALKER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_VORTIGAUNT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_ZOMBIE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_PLAYER_ALLY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_PLAYER_ALLY_VITAL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_GUNSHIP, CLASS_HACKED_ROLLERMINE, D_HT, 0 );

	// ------------------------------------------------------------
	//	> CLASS_COMBINE_HUNTER
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_PLAYER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_CITIZEN_PASSIVE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_CITIZEN_REBEL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_COMBINE, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_COMBINE_GUNSHIP, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_COMBINE_HUNTER, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_CONSCRIPT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_HEADCRAB, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_MANHACK, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_METROPOLICE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_MILITARY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_STALKER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_VORTIGAUNT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_ZOMBIE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_PLAYER_ALLY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_PLAYER_ALLY_VITAL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_COMBINE_HUNTER, CLASS_HACKED_ROLLERMINE, D_HT, 0 );

	// ------------------------------------------------------------
	//	> CLASS_CONSCRIPT
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_PLAYER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_BARNACLE, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_CITIZEN_PASSIVE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_CITIZEN_REBEL, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_COMBINE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_COMBINE_HUNTER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_CONSCRIPT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_HEADCRAB, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_MANHACK, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_METROPOLICE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_MILITARY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_SCANNER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_STALKER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_VORTIGAUNT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_ZOMBIE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_PLAYER_ALLY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_PLAYER_ALLY_VITAL, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_CONSCRIPT, CLASS_HACKED_ROLLERMINE, D_NU, 0 );

	// ------------------------------------------------------------
	//	> CLASS_FLARE
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_PLAYER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_ANTLION, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_CITIZEN_PASSIVE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_CITIZEN_REBEL, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_COMBINE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_COMBINE_HUNTER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_CONSCRIPT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_HEADCRAB, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_MANHACK, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_METROPOLICE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_MILITARY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_STALKER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_VORTIGAUNT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_ZOMBIE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_PLAYER_ALLY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_PLAYER_ALLY_VITAL, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_FLARE, CLASS_HACKED_ROLLERMINE, D_NU, 0 );

	// ------------------------------------------------------------
	//	> CLASS_HEADCRAB
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_PLAYER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_CITIZEN_PASSIVE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_CITIZEN_REBEL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_COMBINE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_COMBINE_HUNTER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_CONSCRIPT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_HEADCRAB, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_MANHACK, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_METROPOLICE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_MILITARY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_STALKER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_VORTIGAUNT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_ZOMBIE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_PLAYER_ALLY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_PLAYER_ALLY_VITAL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HEADCRAB, CLASS_HACKED_ROLLERMINE, D_FR, 0 );

	// ------------------------------------------------------------
	//	> CLASS_MANHACK
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_PLAYER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_CITIZEN_PASSIVE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_CITIZEN_REBEL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_COMBINE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_COMBINE_HUNTER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_CONSCRIPT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_HEADCRAB, D_HT, -1 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_MANHACK, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_METROPOLICE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_MILITARY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_STALKER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_VORTIGAUNT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_ZOMBIE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_PLAYER_ALLY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_PLAYER_ALLY_VITAL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MANHACK, CLASS_HACKED_ROLLERMINE, D_HT, 0 );

	// ------------------------------------------------------------
	//	> CLASS_METROPOLICE
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_PLAYER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_CITIZEN_PASSIVE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_CITIZEN_REBEL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_COMBINE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_COMBINE_HUNTER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_CONSCRIPT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_HEADCRAB, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_MANHACK, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_METROPOLICE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_MILITARY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_STALKER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_VORTIGAUNT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_ZOMBIE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_PLAYER_ALLY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_PLAYER_ALLY_VITAL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_METROPOLICE, CLASS_HACKED_ROLLERMINE, D_HT, 0 );

	// ------------------------------------------------------------
	//	> CLASS_MILITARY
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_PLAYER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_CITIZEN_PASSIVE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_CITIZEN_REBEL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_COMBINE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_COMBINE_HUNTER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_CONSCRIPT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_HEADCRAB, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_MANHACK, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_METROPOLICE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_MILITARY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_STALKER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_VORTIGAUNT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_ZOMBIE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_PLAYER_ALLY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_PLAYER_ALLY_VITAL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MILITARY, CLASS_HACKED_ROLLERMINE, D_HT, 0 );

	// ------------------------------------------------------------
	//	> CLASS_MISSILE
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_PLAYER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_CITIZEN_PASSIVE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_CITIZEN_REBEL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_COMBINE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_COMBINE_HUNTER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_CONSCRIPT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_HEADCRAB, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_MANHACK, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_METROPOLICE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_MILITARY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_STALKER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_VORTIGAUNT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_ZOMBIE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_PLAYER_ALLY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_PLAYER_ALLY_VITAL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_MISSILE, CLASS_HACKED_ROLLERMINE, D_HT, 0 );

	// ------------------------------------------------------------
	//	> CLASS_NONE
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_PLAYER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_ANTLION, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_CITIZEN_PASSIVE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_CITIZEN_REBEL, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_COMBINE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_COMBINE_HUNTER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_CONSCRIPT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_HEADCRAB, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_MANHACK, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_METROPOLICE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_MILITARY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_STALKER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_VORTIGAUNT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_ZOMBIE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_PLAYER_ALLY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_PLAYER_ALLY_VITAL, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_NONE, CLASS_HACKED_ROLLERMINE, D_NU, 0 );

	// ------------------------------------------------------------
	//	> CLASS_PLAYER
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_PLAYER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_BARNACLE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_BULLSEYE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_CITIZEN_PASSIVE, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_CITIZEN_REBEL, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_COMBINE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_COMBINE_GUNSHIP, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_COMBINE_HUNTER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_CONSCRIPT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_HEADCRAB, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_MANHACK, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_METROPOLICE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_MILITARY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_SCANNER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_STALKER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_VORTIGAUNT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_ZOMBIE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_PROTOSNIPER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_PLAYER_ALLY, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_PLAYER_ALLY_VITAL, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER, CLASS_HACKED_ROLLERMINE, D_LI, 0 );

	// ------------------------------------------------------------
	//	> CLASS_PLAYER_ALLY
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_PLAYER, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_BARNACLE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_CITIZEN_PASSIVE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_CITIZEN_REBEL, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_COMBINE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_COMBINE_HUNTER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_CONSCRIPT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_HEADCRAB, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_MANHACK, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_METROPOLICE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_MILITARY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_SCANNER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_STALKER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_VORTIGAUNT, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_ZOMBIE, D_FR, 1 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_PROTOSNIPER, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_PLAYER_ALLY, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_PLAYER_ALLY_VITAL, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY, CLASS_HACKED_ROLLERMINE, D_LI, 0 );

	// ------------------------------------------------------------
	//	> CLASS_PLAYER_ALLY_VITAL
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_PLAYER, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_BARNACLE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_CITIZEN_PASSIVE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_CITIZEN_REBEL, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_COMBINE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_COMBINE_HUNTER, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_CONSCRIPT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_HEADCRAB, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_MANHACK, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_METROPOLICE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_MILITARY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_SCANNER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_STALKER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_VORTIGAUNT, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_ZOMBIE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_PROTOSNIPER, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_PLAYER_ALLY, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_PLAYER_ALLY_VITAL, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PLAYER_ALLY_VITAL, CLASS_HACKED_ROLLERMINE, D_LI, 0 );

	// ------------------------------------------------------------
	//	> CLASS_SCANNER
	// ------------------------------------------------------------	
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_PLAYER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_CITIZEN_PASSIVE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_CITIZEN_REBEL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_COMBINE, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_COMBINE_GUNSHIP, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_COMBINE_HUNTER, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_CONSCRIPT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_HEADCRAB, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_MANHACK, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_METROPOLICE, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_MILITARY, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_SCANNER, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_STALKER, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_VORTIGAUNT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_ZOMBIE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_PROTOSNIPER, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_PLAYER_ALLY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_PLAYER_ALLY_VITAL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_SCANNER, CLASS_HACKED_ROLLERMINE, D_HT, 0 );

	// ------------------------------------------------------------
	//	> CLASS_STALKER
	// ------------------------------------------------------------	
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_PLAYER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_CITIZEN_PASSIVE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_CITIZEN_REBEL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_COMBINE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_COMBINE_HUNTER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_CONSCRIPT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_HEADCRAB, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_MANHACK, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_METROPOLICE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_MILITARY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_STALKER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_VORTIGAUNT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_ZOMBIE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_PLAYER_ALLY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_PLAYER_ALLY_VITAL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_STALKER, CLASS_HACKED_ROLLERMINE, D_HT, 0 );

	// ------------------------------------------------------------
	//	> CLASS_VORTIGAUNT
	// ------------------------------------------------------------	
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_PLAYER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_BARNACLE, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_CITIZEN_PASSIVE, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_CITIZEN_REBEL, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_COMBINE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_COMBINE_HUNTER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_CONSCRIPT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_HEADCRAB, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_MANHACK, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_METROPOLICE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_MILITARY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_SCANNER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_STALKER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_VORTIGAUNT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_ZOMBIE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_PLAYER_ALLY, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_PLAYER_ALLY_VITAL, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_VORTIGAUNT, CLASS_HACKED_ROLLERMINE, D_LI, 0 );

	// ------------------------------------------------------------
	//	> CLASS_ZOMBIE
	// ------------------------------------------------------------	
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_PLAYER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_CITIZEN_PASSIVE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_CITIZEN_REBEL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_COMBINE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_COMBINE_HUNTER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_CONSCRIPT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_HEADCRAB, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_MANHACK, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_METROPOLICE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_MILITARY, D_FR, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_STALKER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_VORTIGAUNT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_ZOMBIE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_PLAYER_ALLY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_PLAYER_ALLY_VITAL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_ZOMBIE, CLASS_HACKED_ROLLERMINE, D_HT, 0 );

	// ------------------------------------------------------------
	//	> CLASS_PROTOSNIPER
	// ------------------------------------------------------------	
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_PLAYER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_CITIZEN_PASSIVE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_CITIZEN_REBEL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_COMBINE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_COMBINE_HUNTER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_CONSCRIPT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_HEADCRAB, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_MANHACK, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_METROPOLICE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_MILITARY, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_MISSILE, D_NU, 5 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_STALKER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_VORTIGAUNT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_ZOMBIE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_PLAYER_ALLY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_PLAYER_ALLY_VITAL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_PROTOSNIPER, CLASS_HACKED_ROLLERMINE, D_HT, 0 );

	// ------------------------------------------------------------
	//	> CLASS_EARTH_FAUNA
	//
	// Hates pretty much everything equally except other earth fauna.
	// This will make the critter choose the nearest thing as its enemy.
	// ------------------------------------------------------------	
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_NONE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_PLAYER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_CITIZEN_PASSIVE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_CITIZEN_REBEL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_COMBINE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_COMBINE_GUNSHIP, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_COMBINE_HUNTER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_CONSCRIPT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_FLARE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_HEADCRAB, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_MANHACK, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_METROPOLICE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_MILITARY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_MISSILE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_SCANNER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_STALKER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_VORTIGAUNT, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_ZOMBIE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_PROTOSNIPER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_EARTH_FAUNA, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_PLAYER_ALLY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_PLAYER_ALLY_VITAL, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_EARTH_FAUNA, CLASS_HACKED_ROLLERMINE, D_NU, 0 );

	// ------------------------------------------------------------
	//	> CLASS_HACKED_ROLLERMINE
	// ------------------------------------------------------------
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_NONE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_PLAYER, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_ANTLION, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_BARNACLE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_BULLSEYE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_CITIZEN_PASSIVE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_CITIZEN_REBEL, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_COMBINE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_COMBINE_GUNSHIP, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_COMBINE_HUNTER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_CONSCRIPT, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_FLARE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_HEADCRAB, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_MANHACK, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_METROPOLICE, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_MILITARY, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_MISSILE, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_SCANNER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_STALKER, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_VORTIGAUNT, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_ZOMBIE, D_HT, 1 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_PROTOSNIPER, D_NU, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_EARTH_FAUNA, D_HT, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_PLAYER_ALLY, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_PLAYER_ALLY_VITAL, D_LI, 0 );
	CBaseCombatCharacter::SetDefaultRelationship( CLASS_HACKED_ROLLERMINE, CLASS_HACKED_ROLLERMINE, D_LI, 0 );
}

CTacticalMissionManager *CGameRules::TacticalMissionManagerFactory( void )
{
	return new CTacticalMissionManager;
}

#endif
